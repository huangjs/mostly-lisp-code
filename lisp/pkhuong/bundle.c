#define RUNME /*
gcc -std=gnu99 -O2 -ggdb $CFLAGS -W -Wall -L/home/cplex/10.0/i686-pc-linux-gnu/lib/ -I/home/cplex/10.0/include *.c -lilocplex -lcplex -lm -lpthread -lc -o bundle
exit $?
*/
#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <ilcplex/cplex.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>

static const double eps = 5e-7;

#define assert_(EXPR)                                           \
        ({                                                      \
                __typeof__(EXPR) __ret = (EXPR);                \
                if (!__ret) {                                   \
                        perror(NULL);                           \
                        assert(0);                              \
                }                                               \
                __ret;                                          \
        })

#define CPX(CALL)             \
        do {                                                            \
                int status = (CPX ## CALL);                             \
                if (status) {                                           \
                        fprintf(stderr, "CPX error: %i\n", status);     \
                        assert(0);                                      \
                }                                                       \
        } while (0)

#define CPX_(CALL)                                                  \
        ({                                                          \
                int status = -1;                                    \
                __typeof__(CPX ## CALL) __ret = (CPX ## CALL);      \
                if (status) {                                       \
                        fprintf(stderr, "CPX error: %i\n", status); \
                        assert(0);                                  \
                }                                                   \
                __ret;                                              \
        })

CPXENVptr env = 0;

void init_cplex ()
{
        env = CPX_(openCPLEX(&status));
}

void destroy_cplex ()
{
        CPX(closeCPLEX(&env));
}

typedef struct {
        double opening_cost;
        double arc_cost[];
} facility_t;

typedef struct {
        size_t nfacilities;
        size_t ncities;
        facility_t ** facilities;
} problem_t;

void init_problem (problem_t * instance, size_t nfacilities, size_t ncities)
{
        instance->nfacilities = nfacilities;
        instance->ncities     = ncities;
        facility_t ** facilities
                = assert_(malloc(sizeof(facility_t*)*nfacilities));

        size_t facility_size = 
                sizeof(facility_t) + ncities * sizeof(double);

        for (size_t i = 0; i < nfacilities; i++)
                bzero(facilities[i] = assert_(malloc(facility_size)),
                      facility_size);

        instance->facilities = facilities;
}

problem_t read_problem (FILE * in)
{
        size_t nfacilities, ncities;
        assert_(2 == fscanf(in, "%i %i", &nfacilities, &ncities));
        problem_t instance;
        init_problem(&instance, nfacilities, ncities);

        for (size_t i = 0; i < nfacilities; i++) {
                double cap, cost;
                assert_(2 == fscanf(in, "%lf %lf", &cap, &cost));
                instance.facilities[i]->opening_cost = cost;
        }

        for (size_t i = 0; i < ncities; i++) {
                double demand;
                assert_(1 == fscanf(in, "%lf", &demand));
                for (size_t j = 0; j < nfacilities; j++) {
                        double connection_cost;
                        assert_(1 == fscanf(in, "%lf", &connection_cost));
                        instance.facilities[j]->arc_cost[i]
                                = connection_cost;
                }
        }

        return instance;
}

typedef struct {
        size_t nmultipliers;
        CPXLPptr lp;
        size_t nconstraints;
        size_t * constraint_age;
        double region_size;
        double * best_multipliers;
        double * best_subgradients;
        double best_value;
        problem_t problem;
} bundle_t;

void init_bundle (bundle_t * bundle, size_t nmultipliers)
{
        bundle->nmultipliers = nmultipliers;
        bundle->lp = CPX_(createprob(env, &status, "Dummy"));
        bundle->nconstraints = 0;
        bundle->constraint_age = NULL;
        bundle->region_size = 1.0;
        assert_(bundle->best_multipliers
                = malloc(sizeof(double)*nmultipliers));
        assert_(bundle->best_subgradients
                = malloc(sizeof(double)*nmultipliers));
        bundle->best_value = -1.0/0.0;

        for (size_t i = 0; i < nmultipliers; i++)
                bundle->best_multipliers[i] = 0.0;
        
        CPXchgobjsen(env, bundle->lp, CPX_MAX);
        double one = 1.0;
        double inf = -CPX_INFBOUND;
        double sup = CPX_INFBOUND;
        CPX(newcols(env, bundle->lp,
                    1, &one, &inf, &sup, 
                    NULL, NULL));

        CPX(newcols(env, bundle->lp,
                    nmultipliers, NULL, NULL, NULL,
                    NULL, NULL));
}

void destroy_bundle (bundle_t * bundle)
{
        CPX(freeprob(env, &bundle->lp));
        bundle->lp = NULL;
        free(bundle->best_multipliers);
        bundle->best_multipliers = NULL;
        bundle->nconstraints = bundle->nmultipliers = -1UL;
}

void add_subgradient (bundle_t * bundle,
                      double * x, double z, double * dz)
{
        size_t nmultipliers = bundle->nmultipliers;
        CPXLPptr lp = bundle->lp;

        double z0 = z;
        // BLAS
        for (size_t i = 0; i < nmultipliers; i++)
                z0 -= x[i] * dz[i];

        // Y <= x . dz + z0
        // Y - x . dz <= z0
        {
                int rmatbeg[] = {0};
                int * indices
                        = assert_(malloc(sizeof(int)*(nmultipliers+1)));
                double * coefs
                        = assert_(malloc(sizeof(double)*(nmultipliers+1)));
                int ncoefs = 1;

                indices[0] = 0;
                coefs[0] = 1.0;

                for (size_t i = 0; i < nmultipliers; i++) {
                        if (dz[i] == 0.0) continue;
                        indices[ncoefs] = i+1;
                        coefs[ncoefs]   = -dz[i];
                        ncoefs++;
                }

                CPX(addrows(env, lp, 0, 1, ncoefs,
                            &z0, "L",
                            rmatbeg, indices, coefs, 
                            NULL, NULL));

                free(indices);
                free(coefs);
        }

        bundle->nconstraints++;
        assert_(bundle->constraint_age 
                = realloc(bundle->constraint_age,
                          bundle->nconstraints*sizeof(size_t)));
        bundle->constraint_age[bundle->nconstraints-1] = 0;
}

void del_subgradient(bundle_t * bundle, size_t index)
{
        CPX(delrows(env, bundle->lp, index, index));
        memmove(bundle->constraint_age+index, bundle->constraint_age+index+1,
                bundle->nconstraints-index-1);
        
        if (--bundle->nconstraints) {
                assert_(bundle->constraint_age
                        = realloc(bundle->constraint_age,
                                  bundle->nconstraints*sizeof(size_t)));
        } else {
                free(bundle->constraint_age);
                bundle->constraint_age = NULL;
        }
}

void print_solution (bundle_t * bundle)
{
        size_t nmultipliers = bundle->nmultipliers;
        double val;
        double * x = assert_(malloc(sizeof(double)*(nmultipliers+1)));

        CPX(getobjval(env, bundle->lp, &val));
        CPX(getx(env, bundle->lp, x, 0, nmultipliers));

        assert(fabs(val - x[0]) < eps);

        printf("z: %f\n", x[0]);

        for (size_t i = 0; i < nmultipliers; i++)
                printf("%f ", x[i+1]);
        printf("\n");

        free(x);

        int * constraints 
                = assert_(malloc(sizeof(int)*(bundle->nconstraints)));
        CPX(getbase(env, bundle->lp, NULL, constraints));

        for (size_t i = 0; i < bundle->nconstraints; i++)
                printf("%i %i\n", i, constraints[i]);

        free(constraints);
}

unsigned update_constraint_age (bundle_t * bundle, size_t iteration)
{
        int * constraints 
                = assert_(malloc(sizeof(int)*(bundle->nconstraints)));
        CPX(getbase(env, bundle->lp, NULL, constraints));

        unsigned count = 0;
        for (size_t i = 0; i < bundle->nconstraints; i++)
                if (CPX_AT_LOWER == constraints[i]) {
                        count++;
                        bundle->constraint_age[i] = iteration;
                }

        free(constraints);

        return count;
}

double eval_multipliers (problem_t * problem, double * multipliers,
                         double * OUT_subgradient)
{
        size_t ncities = problem->ncities;
        double * flow = OUT_subgradient;
        double value = 0;
        
        for (size_t i = 0; i < ncities; i++) {
                flow[i] = -1.0;
                value -= multipliers[i];
        }

        for (size_t i = 0; i < problem->nfacilities; i++) {
                facility_t * facility = problem->facilities[i];

                double cur_value = facility->opening_cost;
                for (size_t j = 0; j < ncities; j++) {
                        double delta = facility->arc_cost[j]
                                + multipliers[j];
                        if (delta < 0) cur_value += delta;
                }

                if (cur_value < 0) {
                        value += cur_value;
                        for (size_t j = 0; j < ncities; j++) {
                                double delta = facility->arc_cost[j]
                                        + multipliers[j];
                                if (delta < 0)
                                        flow[j] += 1.0;
                        }
                }
        }

        printf(" city 0: %f/%f ", multipliers[0], flow[0]);

        return value;
}

double upper_bound_multipliers (problem_t * problem, double * multipliers)
{
        size_t ncities = problem->ncities;
        size_t nfacilities = problem->nfacilities;
        double value = 0;
        int * facility_state
                = assert_(calloc(nfacilities, sizeof(int)));

        for (size_t i = 0; i < nfacilities; i++) {
                facility_t * facility = problem->facilities[i];

                double cur_value = facility->opening_cost;
                for (size_t j = 0; j < ncities; j++) {
                        double delta = facility->arc_cost[j]
                                + multipliers[j];
                        if (delta < 0) cur_value += delta;
                }
                if (cur_value < 0) facility_state[i] = 1;
        }

        for (size_t i = 0; i < ncities; i++) {
                double best_cost     = 1.0/0.0;
                size_t best_facility = -1UL;

                double best_closed_cost     = 1.0/0.0;
                size_t best_closed_facility = -1UL;

                for (size_t j = 0; j < nfacilities; j++) {
                        double cost = problem->facilities[j]->arc_cost[i];

                        if (cost < best_cost) {
                                best_closed_cost     = cost;
                                best_closed_facility = j;
                                if (!facility_state[j]) continue;
                                best_cost     = cost;
                                best_facility = j;
                        }
                }

                assert(best_closed_facility != -1UL);

                if (best_facility != -1UL) {
                        facility_state[best_facility] = 2;
                        value += best_cost;
                } else {
                        facility_state[best_closed_facility] = 2;
                        value += best_closed_cost;
                }
        }

        for (size_t i = 0; i < nfacilities; i++)
                if (2 == facility_state[i]) {
                        value += problem->facilities[i]->opening_cost;
                }

        free(facility_state);

        return value;
}

double solve_bundle (bundle_t * bundle)
{
        size_t nmultipliers = bundle->nmultipliers;
        int    * indices 
                = assert_(malloc(sizeof(int)*nmultipliers*2));
        double * bounds  
                = assert_(malloc(sizeof(double)*nmultipliers*2));
        char   * type    
                = assert_(malloc(nmultipliers*2));

        for (size_t i = 0; i < nmultipliers; i++) {
                indices[i*2] = indices[i*2+1] = i+1;
                bounds[i*2] 
                        = bundle->best_multipliers[i] - bundle->region_size;
                type[i*2] = 'L';
                bounds[i*2+1]
                        = bundle->best_multipliers[i] + bundle->region_size;
                type[i*2+1] = 'U';
        }

        CPX(chgbds(env, bundle->lp, nmultipliers*2,
                   indices, type, bounds));
        
        free(type);
        free(bounds);
        free(indices);

        CPX(dualopt(env, bundle->lp));
        {
                int sol_type;
                CPX(solninfo(env, bundle->lp, 
                             NULL, &sol_type, NULL, NULL));
                assert(sol_type != CPX_NO_SOLN);
        }
        double value;
        CPX(getobjval(env, bundle->lp, &value));
        return value;
}

typedef enum {
        UNBOUNDED,
        MAJOR_STEP,
        NULL_STEP,
        PREDICTED_NULL_STEP
} bundle_status_t;

int bundle_step (bundle_t * bundle, size_t iteration, 
                 unsigned * OUT_nbinding, 
                 double * OUT_prediction, double * OUT_actual)
{
        size_t nmultipliers = bundle->nmultipliers;
        double * multipliers = assert_(malloc(sizeof(double)*nmultipliers));
        double guess = 1.0/0.0;

        if (iteration) {
                guess = solve_bundle(bundle);
                CPX(getx(env, bundle->lp, multipliers, 1, nmultipliers));
                printf("it %i ", CPXgetitcnt(env, bundle->lp));
                *OUT_nbinding = update_constraint_age(bundle, iteration);
        } else {
                memcpy(multipliers, bundle->best_multipliers,
                       sizeof(double)*nmultipliers);
                *OUT_nbinding = 0;
        }

        *OUT_prediction = guess;

        double * subgrad = assert_(malloc(sizeof(double)*nmultipliers));
        double value = eval_multipliers(&bundle->problem, multipliers, subgrad);
        * OUT_actual = value;

        printf("value: %f\t(%f) ", value, guess);

        if (value == -1.0/0.0) return UNBOUNDED;

        add_subgradient(bundle, multipliers, value, subgrad);
        bundle->constraint_age[bundle->nconstraints-1] = iteration;

        if (value > bundle->best_value) {
                bundle->best_value = value;
                memcpy(bundle->best_subgradients, subgrad,
                       sizeof(double)*nmultipliers);
                memcpy(bundle->best_multipliers, multipliers, 
                       sizeof(double)*nmultipliers);
                free(subgrad);
                free(multipliers);
                return MAJOR_STEP;
        }

        free(subgrad);
        free(multipliers);

        double delta = fabs(value-guess);
        double max = fmax(1.0, fmax(fabs(value), fabs(guess)));
        if ((delta / max) < eps)
                return PREDICTED_NULL_STEP;
        return NULL_STEP;
}

// FIXME: needs more info?
double compute_trust_region (bundle_t * bundle, int state, 
                             double prev, double guess, double actual)
{
        double roh  = (actual - prev)/(guess - prev);
        double size = bundle->region_size;

        if (roh > 0.95)
                return size * 5;
        if (MAJOR_STEP == state) return size;
        if ((UNBOUNDED == state) || (roh < -5e-2))
                return size * 0.25;
        return size;
}

void clean_bundle (bundle_t * bundle, size_t bundle_size)
{
        printf("clean: %u/%u ", bundle->nconstraints, bundle_size);
        while (bundle->nconstraints > bundle_size) {
                size_t min_age = -1UL;
                size_t index = -1UL;

                for (size_t i = 0; i < bundle->nconstraints; i++)
                        if (bundle->constraint_age[i] < min_age) {
                                min_age = bundle->constraint_age[i];
                                index = i;
                        }

                assert(index != -1UL);

                del_subgradient(bundle, index);
        }
}

void do_iterations (bundle_t * bundle, size_t niter, size_t bundle_size)
{
        for (size_t i = 0; i < niter; i++) {
                printf("it: %i\tregion: %f ", i, bundle->region_size);
                unsigned nbinding;
                double predicted, actual;
                double prev = bundle->best_value;
                int state = bundle_step(bundle, i,
                                        &nbinding,
                                        &predicted, &actual);
                printf("nbinding: %i ", nbinding);
                bundle->region_size 
                        = compute_trust_region
                        (bundle, state, prev, predicted, actual);
                if ((nbinding == bundle->nmultipliers+1) &&
                    PREDICTED_NULL_STEP == state)
                        break;
                if (MAJOR_STEP == state) {
                        double best = bundle->best_value;
                        if ((best - prev)/best < eps) break;
                        clean_bundle(bundle, bundle_size);
                }

                printf("\n");
        }
        printf("\nbest value: %f (%f)\n", 
               bundle->best_value, 
               upper_bound_multipliers(&bundle->problem, 
                                       bundle->best_multipliers));

        printf("multipliers: \n");
        for (size_t i = 0; i < bundle->nmultipliers; i++)
                printf("%f ", bundle->best_subgradients[i]);
        printf("\n");
}

int main (int argc, char** argv)
{
        bundle_t bundle;

        {
                char * filepath = "MO1";
                if (argc > 1)
                        filepath = argv[1];
                
                FILE * in = assert_(fopen(filepath, "r"));
                problem_t instance = read_problem(in);
                assert_(!fclose(in));

                printf("constraints: %lu\n", instance.ncities);
                bundle.problem = instance;
        }

        init_cplex();
        printf("Version: %s\n", CPXversion(env));

        init_bundle(&bundle, bundle.problem.ncities);

        {
                struct rusage usage;
                assert_(!getrusage(RUSAGE_SELF, &usage));
                double start = usage.ru_utime.tv_sec 
                        + usage.ru_utime.tv_usec * 1e-6;
                do_iterations(&bundle, 100000, 2*bundle.nmultipliers);
                assert_(!getrusage(RUSAGE_SELF, &usage));
                double end = usage.ru_utime.tv_sec 
                        + usage.ru_utime.tv_usec * 1e-6;
                printf("Time: %f\n", end-start);
        }

        destroy_cplex();

        return 0;
}
