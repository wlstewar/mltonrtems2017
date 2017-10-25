(*
 * WARNING: This file was automatically generated by MDLGen (v3.0)
 * from the machine description file "alpha/alpha.mdl".
 * DO NOT EDIT this file directly
 *)


signature ALPHACELLS =
sig
   include CELLS
   val CELLSET : CellsBasis.cellkind
   val showGP : CellsBasis.register_id -> string
   val showFP : CellsBasis.register_id -> string
   val showCC : CellsBasis.register_id -> string
   val showMEM : CellsBasis.register_id -> string
   val showCTRL : CellsBasis.register_id -> string
   val showCELLSET : CellsBasis.register_id -> string
   val showGPWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showFPWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showCCWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showMEMWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showCTRLWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val showCELLSETWithSize : CellsBasis.register_id * CellsBasis.sz -> string
   val returnAddr : CellsBasis.cell
   val r31 : CellsBasis.cell
   val f31 : CellsBasis.cell
   val addGP : CellsBasis.cell * cellset -> cellset
   val addFP : CellsBasis.cell * cellset -> cellset
   val addCC : CellsBasis.cell * cellset -> cellset
   val addMEM : CellsBasis.cell * cellset -> cellset
   val addCTRL : CellsBasis.cell * cellset -> cellset
   val addCELLSET : CellsBasis.cell * cellset -> cellset
end

structure AlphaCells : ALPHACELLS =
struct
   exception AlphaCells
   fun error msg = MLRiscErrorMsg.error("AlphaCells",msg)
   open CellsBasis
   fun showGPWithSize (r, ty) = (fn (30, _) => "$sp"
                                  | (r, _) => "$" ^ (Int.toString r)
                                ) (r, ty)
   and showFPWithSize (r, ty) = (fn (f, _) => "$f" ^ (Int.toString f)
                                ) (r, ty)
   and showCCWithSize (r, ty) = (fn _ => "cc"
                                ) (r, ty)
   and showMEMWithSize (r, ty) = (fn (r, _) => "m" ^ (Int.toString r)
                                 ) (r, ty)
   and showCTRLWithSize (r, ty) = (fn (r, _) => "ctrl" ^ (Int.toString r)
                                  ) (r, ty)
   and showCELLSETWithSize (r, ty) = (fn _ => "CELLSET"
                                     ) (r, ty)
   fun showGP r = showGPWithSize (r, 64)
   fun showFP r = showFPWithSize (r, 64)
   fun showCC r = showCCWithSize (r, 64)
   fun showMEM r = showMEMWithSize (r, 8)
   fun showCTRL r = showCTRLWithSize (r, 0)
   fun showCELLSET r = showCELLSETWithSize (r, 0)
   val CELLSET = CellsBasis.newCellKind {name="CELLSET", nickname="cellset"}
   structure MyCells = Cells
      (exception Cells = AlphaCells
       val firstPseudo = 256
       val desc_GP = CellsBasis.DESC {low=0, high=31, kind=CellsBasis.GP, defaultValues=[(31, 
              0)], zeroReg=SOME 31, toString=showGP, toStringWithSize=showGPWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_FP = CellsBasis.DESC {low=32, high=63, kind=CellsBasis.FP, 
              defaultValues=[(63, 0)], zeroReg=SOME 31, toString=showFP, toStringWithSize=showFPWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_MEM = CellsBasis.DESC {low=64, high=63, kind=CellsBasis.MEM, 
              defaultValues=[], zeroReg=NONE, toString=showMEM, toStringWithSize=showMEMWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CTRL = CellsBasis.DESC {low=64, high=63, kind=CellsBasis.CTRL, 
              defaultValues=[], zeroReg=NONE, toString=showCTRL, toStringWithSize=showCTRLWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CELLSET = CellsBasis.DESC {low=64, high=63, kind=CELLSET, defaultValues=[], 
              zeroReg=NONE, toString=showCELLSET, toStringWithSize=showCELLSETWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       val cellKindDescs = [(CellsBasis.GP, desc_GP), (CellsBasis.FP, desc_FP), 
              (CellsBasis.CC, desc_GP), (CellsBasis.MEM, desc_MEM), (CellsBasis.CTRL, 
              desc_CTRL), (CELLSET, desc_CELLSET)]
       val cellSize = 4
      )

   open MyCells
   val addGP = CellSet.add
   and addFP = CellSet.add
   and addCC = CellSet.add
   and addMEM = CellSet.add
   and addCTRL = CellSet.add
   and addCELLSET = CellSet.add
   val RegGP = Reg GP
   and RegFP = Reg FP
   and RegCC = Reg CC
   and RegMEM = Reg MEM
   and RegCTRL = Reg CTRL
   and RegCELLSET = Reg CELLSET
   val stackptrR = RegGP 30
   val asmTmpR = RegGP 28
   val fasmTmp = RegGP 30
   val returnAddr = RegGP 26
   val r31 = RegGP 31
   val f31 = RegFP 31
end

