(LABEL GARBAGE::L2)
(MOVE.Q (:REG 4 :RSP :ESP :SP :SPL) (:REG 10 :R10 :R10D :R10W :R10B))
(SAVE (IM (:FINAL-FRAME-SIZE 112 -8)) (:REG 4 :RSP :ESP :SP :SPL))
(ADD.Q (IM 8) (:REG 10 :R10 :R10D :R10W :R10B))
(MOVE.Q (:REG 10 :R10 :R10D :R10W :R10B)
 (D 0 (:REG 4 :RSP :ESP :SP :SPL)))
(MOVE.Q (:REG 14 :R14 :R14D :R14W :R14B)
 (D 8 (:REG 4 :RSP :ESP :SP :SPL)))
(LABEL GARBAGE::L1)
(MOVE.Q (IM 0) (:LOCAL 0))
(LABEL GARBAGE::L5)
(LABEL GARBAGE::L3)
(MOVE.Q (D 54 (:REG 14 :R14 :R14D :R14W :R14B))
 (:REG 13 :R13 :R13D :R13W :R13B))
(XMOVE.D (D -10 (:REG 13 :R13 :R13D :R13W :R13B)) (:XREG 0 "xmm0"))
(MOVE.Q (D 62 (:REG 14 :R14 :R14D :R14W :R14B)) (:LOCAL 2))
(LEA (D 96 (:REG 4 :RSP :ESP :SP :SPL)) (:REG 0 :RAX :EAX :AX :AL))
(MOVE.Q (D 464 (:REG 3 :RBX :EBX :BX :BL))
 (D 0 (:REG 0 :RAX :EAX :AX :AL)))
(MOVE.Q (:REG 4 :RSP :ESP :SP :SPL) (D 16 (:REG 0 :RAX :EAX :AX :AL)))
(MOVE.Q (:REG 15 :R15 :R15D :R15W :R15B)
 (D 24 (:REG 0 :RAX :EAX :AX :AL)))
(PUSH.Q (IM 0))
(POP.Q (D 8 (:REG 0 :RAX :EAX :AX :AL)))
(MOVE.Q (:REG 0 :RAX :EAX :AX :AL) (D 464 (:REG 3 :RBX :EBX :BX :BL)))
(MOVE.Q (:LOCAL 2) (:REG 5 :RBP :EBP :BP :BPL))
(BCC :T (PC (D -2 (:REG 5 :RBP :EBP :BP :BPL))) :LINK)
(MOVE.Q (:REG 0 :RAX :EAX :AX :AL) (:REG 7 :RDI :EDI :DI :DIL))
(MOVE.Q (D 464 (:REG 3 :RBX :EBX :BX :BL)) (:REG 0 :RAX :EAX :AX :AL))
(CMP.Q (:REG 15 :R15 :R15D :R15W :R15B)
 (D 24 (:REG 0 :RAX :EAX :AX :AL)))
(BCC :EQ GARBAGE::L6)
(BCC :T (PC (D 24 (:REG 0 :RAX :EAX :AX :AL))))
(LABEL GARBAGE::L6)
(MOVE.Q (D 0 (:REG 0 :RAX :EAX :AX :AL))
 (D 464 (:REG 3 :RBX :EBX :BX :BL)))
(ADD.Q (IM 8) (:LOCAL 0))
(MOVE.Q (:LOCAL 0) (:REG 13 :R13 :R13D :R13W :R13B))
(CMP.Q (IM 800000000) (:REG 13 :R13 :R13D :R13W :R13B))
(BCC :LT GARBAGE::L5)
(LABEL GARBAGE::L4)
(MOVE.Q (:REG 15 :R15 :R15D :R15W :R15B) (:REG 7 :RDI :EDI :DI :DIL))
(CLC)
(LEA (D (:FINAL-FRAME-SIZE 112 -8) (:REG 4 :RSP :ESP :SP :SPL))
 (:REG 4 :RSP :ESP :SP :SPL))
(MOVE.Q (D 16 (:REG 4 :RSP :ESP :SP :SPL))
 (:REG 14 :R14 :R14D :R14W :R14B))
(RETURN)