#!/usr/bin/python

## TODO:
## Check that supply drop can't be used - w/o mana
##                                      - > # depot
## Same for OC VS SCV

from gurobipy import *
setParam("Threads", 11)

m = Model("mip")
#m.setParam("MIPFocus", 2) # prove optimality!
m.setParam("MIPGapAbs", 5)
horizon = 332
step = 1
times = range(0, horizon, step)

# decision variables model a a discrete time simulation
periods = list()
for time in times:
    periods.append(dict())

# previous values are cached to provide starting solutions to the MIP
previous_values = list()
for time in times:
    previous_values.append(dict())

tps = zip(times, periods)

for p in periods:
    p["mineral"]  = m.addVar(0, 10000, 0, GRB.CONTINUOUS)
    p["dmin"]     = m.addVar(-10000, 10000, 0, GRB.CONTINUOUS) # prod since last event
    p["scv"]      = m.addVar(0, 30, 0, GRB.CONTINUOUS)
    p["scv.mining"]   = m.addVar(0, 18, 0, GRB.CONTINUOUS)
    p["scv.building"] = m.addVar(0, 30, 0, GRB.CONTINUOUS)
    p["scv.scouting"] = m.addVar(0, 1, 0, GRB.INTEGER)
    p["scv.prod"] = m.addVar(0, 1, 0, GRB.INTEGER)
    p["supply"]   = m.addVar(0, 200, 0, GRB.CONTINUOUS)
    p["supply_usage"]   = m.addVar(0, 200, 0, GRB.CONTINUOUS)
    p["depot"]    = m.addVar(0, 5, 0, GRB.INTEGER)
    p["depot.drop"] = m.addVar(0, 5, 0, GRB.INTEGER)
    p["depot.prod"]= m.addVar(0, 1, 0, GRB.BINARY)
    p["depot.built"]= m.addVar(0, 1, 0, GRB.BINARY)
    p["rax"]   = m.addVar(0, 4, 0, GRB.CONTINUOUS)
    p["rax.prod"]= m.addVar(0, 4, 0, GRB.INTEGER)
    p["marine"]   = m.addVar(0, 20, 0, GRB.CONTINUOUS)
    p["marine.prod"]= m.addVar(0, 4, 0, GRB.INTEGER)
    p["oc"]    = m.addVar(0, 1, 0, GRB.BINARY)
    p["oc.prod"] = m.addVar(0, 1, 0, GRB.BINARY)
    p["oc.mana"] = m.addVar(0, 100, 0, GRB.CONTINUOUS)
    p["oc.drop"] = m.addVar(0, 1, 0, GRB.BINARY)

for (prev, p) in zip(previous_values, periods):
    for key in p:
        prev[key] = GRB.UNDEFINED

# set of variables that shouldn't be printed
noprint = set(["supply", "supply_usage", "scv.mining", "scv.building", "depot.built", "mineral", "dmin", "oc.mana", "oc.drop"])
# set of variables that should be printed on change
edge_triggered = set(["scv", "rax", "marine", "depot", "oc", "depot.drop", "scv.scouting"])

m.update()

def blit_previous_values(prev = previous_values):
    for (p, prev) in zip(periods, prev):
        for key in p:
            p[key].Start = prev[key]

def clique_constraint(name, lower, upper = None):
    lhs = LinExpr()
    for p in periods:
        lhs.addTerms(1, p[name])
    if lower == upper:
        m.addConstr(lhs, GRB.EQUAL, lower)
    else:
        m.addConstr(lhs, GRB.GREATER_EQUAL, lower)
        if upper != None:
            m.addConstr(lhs, GRB.LESS_EQUAL, upper)

def use_scv (prod_info, time):
    scv_usage = LinExpr()
    for (name, delay) in prod_info:
        for (t, p) in tps:
            if (t <= time < t+delay):
                scv_usage.addTerms(1, p[name])
    return scv_usage

def use_supply (supply_info, time, now):
    usage = LinExpr()
    for (name, name_prod, delay, weight) in supply_info:
        usage.addTerms(weight, now[name])
        for (t, p) in tps:
            if (t <= time < t+delay):
                usage.addTerms(weight, p[name_prod])
    m.addConstr(now["supply_usage"], GRB.EQUAL, usage)

def update_var_expr (name, prod, delay, weight, time, prev_time, prev):
    total = LinExpr(1, prev[name])
    for (t2, p2) in tps:
        if (prev_time < t2+delay <= time):
            total.addTerms(weight, p2[prod])
    return total

def update_var (name, prod, delay, weight, time, now, prev_time, prev):
    m.addConstr(now[name], GRB.EQUAL, update_var_expr(name, prod, delay, weight, time, prev_time, prev))

def any_built_expr (prod, delay, weight, time, now, prev_time, prev):
    total = LinExpr()
    for (t2, p2) in tps:
        if (prev_time < t2+delay <= time):
            total.addTerms(weight, p2[prod])
    return total

def update_built_var (built, prod, delay, time, now, prev):
    total = LinExpr()
    for (t2, p2) in tps:
        if (t2+delay <= time):
            total.addTerms(1.0, p2[prod])
    m.addConstr(now[built], GRB.LESS_EQUAL, total)
    if (prev != None):
        m.addConstr(now[built], GRB.GREATER_EQUAL, prev[built])

def buy_dmin (info, now):
    total = LinExpr()
    for (name, price) in info:
        total.addTerms(-price, now[name])
    return total

def production_cap (info, now, t):
    for (name, delay, cap) in info:
        total = LinExpr(1, now[name])
        for (t2, p2) in tps:
            if (t2 < t < t2+delay):
                total.addTerms(1, p2[name])
        m.addConstr(total, GRB.LESS_EQUAL, cap)

def multi_production_cap (info, cap, t):
    total = LinExpr()
    for (name, delay) in info:
        for (t2, p2) in tps:
            if (t2 <= t < t2+delay):
                total.addTerms(1, p2[name])
    m.addConstr(total, GRB.LESS_EQUAL, cap)

# Refactor this into a specialised modeling interface some time soon
prev = None
prev_time = None
for (t, p) in tps:
    dmin = LinExpr()
    oc_mana = LinExpr()
    m.addConstr(p["scv"], GRB.EQUAL, LinExpr([1, 1, 1], [p["scv.mining"], p["scv.building"], p["scv.scouting"]]))
    m.addConstr(p["scv.building"], GRB.EQUAL, use_scv([("depot.prod", 30), ("rax.prod", 60)], t))
    use_supply([("scv", "scv.prod", 17, 1), ("marine", "marine.prod", 25, 1)], t, p)
    m.addConstr(p["rax.prod"], GRB.LESS_EQUAL, LinExpr(4, p["depot"]))
    m.addConstr(p["depot.drop"], GRB.LESS_EQUAL, p["depot"])
    m.addConstr(p["oc.prod"], GRB.LESS_EQUAL, p["rax"])
    m.addConstr(p["oc.drop"], GRB.LESS_EQUAL, p["oc"])
    supply = LinExpr()
    supply.addConstant(11)
    supply.addTerms([8, 8], [p["depot"], p["depot.drop"]])
    m.addConstr(p["supply"], GRB.LESS_EQUAL, supply)
    m.addConstr(p["supply_usage"], GRB.LESS_EQUAL, p["supply"])
    # force scouting
    if (90 < t < 180):
        m.addConstr(p["scv.scouting"], GRB.EQUAL, 1)
    else:
        m.addConstr(p["scv.scouting"], GRB.EQUAL, 0)
    if (prev == None):
        mineral = LinExpr(50)
        mineral.addTerms(1.0, p["dmin"])
        m.addConstr(p["mineral"], GRB.EQUAL, mineral)
        m.addConstr(p["scv"], GRB.EQUAL, 6)
        m.addConstr(p["rax"], GRB.EQUAL, 0)
        m.addConstr(p["marine"], GRB.EQUAL, 0)
        m.addConstr(p["oc"], GRB.EQUAL, 0)
        m.addConstr(p["oc.mana"], GRB.EQUAL, 0)
        m.addConstr(p["depot"], GRB.EQUAL, 0)
        m.addConstr(p["depot.drop"], GRB.EQUAL, 0)
    else:
        m.addConstr(p["mineral"], GRB.EQUAL, LinExpr([1.0, 1.0], [prev["mineral"], p["dmin"]]))
        update_var("scv", "scv.prod", 17, 1, t, p, prev_time, prev)

        update_var("depot", "depot.prod", 30, 1, t, p, prev_time, prev)
        update_var("rax", "rax.prod", 60, 1, t, p, prev_time, prev)
        update_var("marine", "marine.prod", 25, 1, t, p, prev_time, prev)
        update_var("oc", "oc.prod", 35, 1, t, p, prev_time, prev)

        dmin.addTerms((t-prev_time)*(40.0/60), prev["scv.mining"])

        oc_mana.addTerms(1, prev["oc.mana"])
        oc_mana.addTerms((t-prev_time)*.56, prev["oc"])
        oc_mana.add(any_built_expr("oc.prod", 35, 50, t, p, prev_time, prev))
        oc_mana.addTerms(-50, p["oc.drop"])
        m.addConstr(p["oc.mana"], GRB.LESS_EQUAL, oc_mana)
        m.addConstr(p["depot.drop"], GRB.EQUAL, LinExpr([1, 1], [prev["depot.drop"], p["oc.drop"]]))

    dmin.add(buy_dmin([("scv.prod", 50), ("depot.prod", 100), ("rax.prod", 150), ("marine.prod", 50), ("oc.prod", 150)], p))
    production_cap([("scv.prod", 17, 1), ("marine.prod", 25, p["rax"])], p, t)
    multi_production_cap([("scv.prod", 17), ("oc.prod", 35)], 1, t)
    m.addConstr(p["dmin"], GRB.EQUAL, dmin)
    prev_time = t
    prev = p

#clique_constraint("oc.prod", 1, 1)
#clique_constraint("depot.prod", 1, 2)
#clique_constraint("rax.prod", 1, 3)
#clique_constraint("marine.prod", 8, 15)

oc_constraint = None

def setup_obj (time, oc = 0):
    global oc_constraint
    if (oc_constraint != None):
        m.remove(oc_constraint)
    if oc == 0:
        oc_constraint = None
    if oc == 1:
        oc_built = LinExpr()
        for t in times:
            if (t > time): break
            oc_built.addTerms(1, periods[t]["oc.prod"])
        oc_constraint = m.addConstr(oc_built, GRB.EQUAL, 1)
    elif oc == 2:
        oc_constraint = m.addConstr(periods[time]["oc"], GRB.EQUAL, 1)

    obj = LinExpr()
    obj.addTerms(100, periods[time]["marine"])
    for (t, p) in tps:
        if (time-25 < t <= time):
            obj.addTerms(1+time-t, p["marine.prod"])
    m.setObjective(obj, -1)

def print_build(time):
    print 'Build for t =', time
    prev = None
    for (t, p) in tps:
        if (t > time): break
        out = ''
        for (key, var) in p.iteritems():
            if (var.X > 1e-1) \
                    and (key not in noprint) \
                    and ((prev == None) \
                             or t == time \
                             or key not in edge_triggered \
                             or (abs(var.X - prev[key].X) > 1e-1)):
                out += key + ":" + `round(var.X)` + "\t"
        if (len(out) > 0):
            print 'Time: ', t, "\tsupply: ", round(p["supply_usage"].X), "\tmineral: ", round(p["mineral"].X), \
                "\tmana", p["oc.mana"].X, "\t", out
        prev = p
    print 'Time ', time, 'obj: ', m.ObjVal

values = list()
for time in range(180, 331, 10):
    try:
        setup_obj(time)
        blit_previous_values()
        m.optimize()
        print_build(time)
        values.append((time, m.ObjVal))
    except:
        print 'Error at time =', time

print 'Values'
for (time, value) in values:
    print '\t', time, '\t', value
