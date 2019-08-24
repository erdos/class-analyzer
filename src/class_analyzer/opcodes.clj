(ns class-analyzer.opcodes)

;; https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-7.html

;; byte sizes: https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings

;; code number to descr
(def instructions {})

(defmacro ^:private codetable [& elems]
  (->> (for [[n _ code] (partition 3 elems)]
         [(int n) (keyword code)])
       (into {})
       (list 'def 'codes)))

(defmacro instruction [opcode hex-opcode mnemonic args]
  (assert (= opcode hex-opcode))
  (assert (keyword? mnemonic))
  ;; ez lehet vektor is, ekkor az erteke:
  ;; 1 + sum( int -> 4, short -> 2, byte->1)
  ;; (assert (pos? size-in-bytes))
  (assert (or (= :special args) (vector? args)))
  `(alter-var-root #'instructions assoc ~opcode
                   {:mnemonic ~mnemonic, :args ~args}))


(instruction 0  0x0 :nop [])
(instruction 1  0x1 :aconst_null [])
(instruction 2  0x2 :iconst_m1 [])
(instruction 3  0x3 :iconst_0 [])
(instruction 4  0x4 :iconst_1 [])
(instruction 5  0x5 :iconst_2 [])
(instruction 6  0x6 :iconst_3 [])
(instruction 7  0x7 :iconst_4 [])
(instruction 8  0x8 :iconst_5 [])
(instruction 9  0x9 :lconst_0 [])
(instruction 10 0xa :lconst_1 [])
(instruction 11 0xb :fconst_0 [])
(instruction 12 0xc :fconst_1 [])
(instruction 13 0xd :fconst_2 [])
(instruction 14 0xe :dconst_0 [])
(instruction 15 0xf :dconst_1 [])

(instruction 16 0x10 :bipush [:byte]) ;; one byte embedded constant
(instruction 17 0x11 :sipush [:short])
(instruction 18 0x12 :ldc [:cpidx1]) ;; one byte embedded constant pool idx

(instruction 19 0x13 :ldc_w [:cpidx2])
(instruction 20 0x14 :ldc2_w [:cpidx2])

(instruction 21 0x15 :iload [:byte]) ;; one byte embedded local variable idx
(instruction 22 0x16 :lload [:byte])
(instruction 23 0x17 :fload [:byte])
(instruction 24 0x18 :dload [:byte])
(instruction 25 0x19 :aload [:byte])

(instruction 26 0x1a :iload_0 [])
(instruction 27 0x1b :iload_1 [])
(instruction 28 0x1c :iload_2 [])
(instruction 29 0x1d :iload_3 [])
(instruction 30 0x1e :lload_0 [])
(instruction 31 0x1f :lload_1 [])
(instruction 32 0x20 :lload_2 [])
(instruction 33 0x21 :lload_3 [])
(instruction 34 0x22 :fload_0 [])
(instruction 35 0x23 :fload_1 [])
(instruction 36 0x24 :fload_2 [])
(instruction 37 0x25 :fload_3 [])
(instruction 38 0x26 :dload_0 [])
(instruction 39 0x27 :dload_1 [])
(instruction 40 0x28 :dload_2 [])
(instruction 41 0x29 :dload_3 [])
(instruction 42 0x2a :aload_0 [])
(instruction 43 0x2b :aload_1 [])
(instruction 44 0x2c :aload_2 [])
(instruction 45 0x2d :aload_3 [])
(instruction 46 0x2e :iaload [])
(instruction 47 0x2f :laload [])
(instruction 48 0x30 :faload [])
(instruction 49 0x31 :daload [])
(instruction 50 0x32 :aaload [])
(instruction 51 0x33 :baload [])
(instruction 52 0x34 :caload [])
(instruction 53 0x35 :saload [])

(instruction 54 0x36 :istore [:byte]) ;; one byte embedded local var idx
(instruction 55 0x37 :lstore [:byte])
(instruction 56 0x38 :fstore [:byte])
(instruction 57 0x39 :dstore [:byte])
(instruction 58 0x3a :astore [:byte])

(instruction 59 0x3b :istore_0 [])
(instruction 60 0x3c :istore_1 [])
(instruction 61 0x3d :istore_2 [])
(instruction 62 0x3e :istore_3 [])
(instruction 63 0x3f :lstore_0 [])
(instruction 64 0x40 :lstore_1 [])
(instruction 65 0x41 :lstore_2 [])
(instruction 66 0x42 :lstore_3 [])
(instruction 67 0x43 :fstore_0 [])
(instruction 68 0x44 :fstore_1 [])
(instruction 69 0x45 :fstore_2 [])
(instruction 70 0x46 :fstore_3 [])
(instruction 71 0x47 :dstore_0 [])
(instruction 72 0x48 :dstore_1 [])
(instruction 73 0x49 :dstore_2 [])
(instruction 74 0x4a :dstore_3 [])
(instruction 75 0x4b :astore_0 [])
(instruction 76 0x4c :astore_1 [])
(instruction 77 0x4d :astore_2 [])
(instruction 78 0x4e :astore_3 [])
(instruction 79 0x4f :iastore [])
(instruction 80 0x50 :lastore [])
(instruction 81 0x51 :fastore [])
(instruction 82 0x52 :dastore [])
(instruction 83 0x53 :aastore [])
(instruction 84 0x54 :bastore [])
(instruction 85 0x55 :castore [])
(instruction 86 0x56 :sastore [])
(instruction 87 0x57 :pop [])
(instruction 88 0x58 :pop2 [])
(instruction 89 0x59 :dup [])
(instruction 90 0x5a :dup_x1 [])
(instruction 91 0x5b :dup_x2 [])
(instruction 92 0x5c :dup2 [])
(instruction 93 0x5d :dup2_x1 [])
(instruction 94 0x5e :dup2_x2 [])
(instruction 95 0x5f :swap [])
(instruction 96 0x60 :iadd [])
(instruction 97 0x61 :ladd [])
(instruction 98 0x62 :fadd [])
(instruction 99 0x63 :dadd [])
(instruction 100 0x64 :isub [])
(instruction 101 0x65 :lsub [])
(instruction 102 0x66 :fsub [])
(instruction 103 0x67 :dsub [])
(instruction 104 0x68 :imul [])
(instruction 105 0x69 :lmul [])
(instruction 106 0x6a :fmul [])
(instruction 107 0x6b :dmul [])
(instruction 108 0x6c :idiv [])
(instruction 109 0x6d :ldiv [])
(instruction 110 0x6e :fdiv [])
(instruction 111 0x6f :ddiv [])
(instruction 112 0x70 :irem [])
(instruction 113 0x71 :lrem [])
(instruction 114 0x72 :frem [])
(instruction 115 0x73 :drem [])
(instruction 116 0x74 :ineg [])
(instruction 117 0x75 :lneg [])
(instruction 118 0x76 :fneg [])
(instruction 119 0x77 :dneg [])
(instruction 120 0x78 :ishl [])
(instruction 121 0x79 :lshl [])
(instruction 122 0x7a :ishr [])
(instruction 123 0x7b :lshr [])
(instruction 124 0x7c :iushr [])
(instruction 125 0x7d :lushr [])
(instruction 126 0x7e :iand [])
(instruction 127 0x7f :land [])
(instruction 128 0x80 :ior  [])
(instruction 129 0x81 :lor  [])
(instruction 130 0x82 :ixor [])
(instruction 131 0x83 :lxor [])

(instruction 132 0x84 :iinc [:byte :byte]) ;; one byte local var idx, other for the constant

(instruction 133 0x85 :i2l [])
(instruction 134 0x86 :i2f [])
(instruction 135 0x87 :i2d [])
(instruction 136 0x88 :l2i [])
(instruction 137 0x89 :l2f [])
(instruction 138 0x8a :l2d [])
(instruction 139 0x8b :f2i [])
(instruction 140 0x8c :f2l [])
(instruction 141 0x8d :f2d [])
(instruction 142 0x8e :d2i [])
(instruction 143 0x8f :d2l [])
(instruction 144 0x90 :d2f [])
(instruction 145 0x91 :i2b [])
(instruction 146 0x92 :i2c [])
(instruction 147 0x93 :i2s [])
(instruction 148 0x94 :lcmp [])
(instruction 149 0x95 :fcmpl [])
(instruction 150 0x96 :fcmpg [])
(instruction 151 0x97 :dcmpl [])
(instruction 152 0x98 :dcmpg [])

(instruction 153 0x99 :ifeq [:branchoffset])
(instruction 154 0x9a :ifne [:branchoffset])
(instruction 155 0x9b :iflt [:branchoffset])
(instruction 156 0x9c :ifge [:branchoffset])
(instruction 157 0x9d :ifgt [:branchoffset])
(instruction 158 0x9e :ifle [:branchoffset])

(instruction 159 0x9f :if_icmpeq [:branchoffset])
(instruction 160 0xa0 :if_icmpne [:branchoffset])
(instruction 161 0xa1 :if_icmplt [:branchoffset])
(instruction 162 0xa2 :if_icmpge [:branchoffset])
(instruction 163 0xa3 :if_icmpgt [:branchoffset])
(instruction 164 0xa4 :if_icmple [:branchoffset])
(instruction 165 0xa5 :if_acmpeq [:branchoffset])
(instruction 166 0xa6 :if_acmpne [:branchoffset])
(instruction 167 0xa7 :goto [:branchoffset])
(instruction 168 0xa8 :jsr [:branchoffset])

(instruction 169 0xa9 :ret [:byte]) ;; one embedded local var idx


(instruction 170 0xaa :tableswitch :special)
(instruction 171 0xab :lookupswitch :special)


(instruction 172 0xac :ireturn [])
(instruction 173 0xad :lreturn [])
(instruction 174 0xae :freturn [])
(instruction 175 0xaf :dreturn [])
(instruction 176 0xb0 :areturn [])
(instruction 177 0xb1 :return  [])

(instruction 178 0xb2 :getstatic [:short])
(instruction 179 0xb3 :putstatic [:short])
(instruction 180 0xb4 :getfield [:short])
(instruction 181 0xb5 :putfield [:short])
(instruction 182 0xb6 :invokevirtual [:cpidx2])
(instruction 183 0xb7 :invokespecial [:cpidx2])
(instruction 184 0xb8 :invokestatic [:cpidx2])

(instruction 185 0xb9 :invokeinterface [:cpidx2 :byte :zerobyte])
(instruction 186 0xba :invokedynamic [:cpidx2 :zerobyte :zerobyte])

(instruction 187 0xbb :new [:cpidx2])

(instruction 188 0xbc :newarray [:byte]) ;; primitive type code
(instruction 189 0xbd :anewarray [:cpidx2])
(instruction 190 0xbe :arraylength [])
(instruction 191 0xbf :athrow [])

(instruction 192 0xc0 :checkcast [:cpidx2])
(instruction 193 0xc1 :instanceof [:cpidx2])
(instruction 194 0xc2 :monitorenter [])
(instruction 195 0xc3 :monitorexit [])

(instruction 196 0xc4 :wide :special)

(instruction 197 0xc5 :multianewarray [:cpidx2 :byte]) ;; type + dimensions
(instruction 198 0xc6 :ifnull [:short]) ;; branchbyte
(instruction 199 0xc7 :ifnonnull [:branchoffset])

(instruction 200 0xc8 :goto_w [:branchoffset4]) ;; four bytes int totals of 5

(instruction 201 0xc9 :jsr_w [:branchoffset4])

;; reserved
; (instruction 202 0xca :breakpoint ?)
; (instruction 254 0xfe :impdep1 ?)
; (instruction 255 0xff :impdep2 ?)

nil
