/* MLton g0b5910a-dirty (built Thu Nov 30 01:30:50 EST 2017 on oem-virtual-machine) */
/*   created this file on Sun Dec 10 05:11:31 2017. */
/* Do not edit this file. */
/* Flag settings:  */
/*    align: 8 */
/*    atMLtons: (@MLton, --) */
/*    chunk: coalesce 4096 */
/*    closureConvertGlobalize: true */
/*    closureConvertShrink: true */
/*    codegen: i386-rtems4.11 */
/*    contifyIntoMain: false */
/*    debug: false */
/*    defaultChar: char8 */
/*    defaultWideChar: widechar32 */
/*    defaultInt: int32 */
/*    defaultReal: real64 */
/*    defaultWord: word32 */
/*    diag passes: [] */
/*    drop passes: [] */
/*    elaborate allowConstant (default): false */
/*    elaborate allowConstant (enabled): true */
/*    elaborate allowFFI (default): false */
/*    elaborate allowFFI (enabled): true */
/*    elaborate allowPrim (default): false */
/*    elaborate allowPrim (enabled): true */
/*    elaborate allowOverload (default): false */
/*    elaborate allowOverload (enabled): true */
/*    elaborate allowOptBar (default): false */
/*    elaborate allowOptBar (enabled): true */
/*    elaborate allowOptSemicolon (default): false */
/*    elaborate allowOptSemicolon (enabled): true */
/*    elaborate allowLineComments (default): false */
/*    elaborate allowLineComments (enabled): true */
/*    elaborate allowDoDecls (default): false */
/*    elaborate allowDoDecls (enabled): true */
/*    elaborate allowRecPunning (default): false */
/*    elaborate allowRecPunning (enabled): true */
/*    elaborate allowOrPats (default): false */
/*    elaborate allowOrPats (enabled): true */
/*    elaborate allowExtendedLiterals (default): false */
/*    elaborate allowExtendedLiterals (enabled): true */
/*    elaborate allowSigWithtype (default): false */
/*    elaborate allowSigWithtype (enabled): true */
/*    elaborate allowRebindEquals (default): false */
/*    elaborate allowRebindEquals (enabled): true */
/*    elaborate deadCode (default): false */
/*    elaborate deadCode (enabled): true */
/*    elaborate forceUsed (default): false */
/*    elaborate forceUsed (enabled): true */
/*    elaborate ffiStr (default):  */
/*    elaborate ffiStr (enabled): true */
/*    elaborate nonexhaustiveExnMatch (default): default */
/*    elaborate nonexhaustiveExnMatch (enabled): true */
/*    elaborate nonexhaustiveMatch (default): warn */
/*    elaborate nonexhaustiveMatch (enabled): true */
/*    elaborate redundantMatch (default): warn */
/*    elaborate redundantMatch (enabled): true */
/*    elaborate resolveScope (default): strdec */
/*    elaborate resolveScope (enabled): true */
/*    elaborate sequenceNonUnit (default): ignore */
/*    elaborate sequenceNonUnit (enabled): true */
/*    elaborate valrecConstr (default): warn */
/*    elaborate valrecConstr (enabled): true */
/*    elaborate warnUnused (default): false */
/*    elaborate warnUnused (enabled): true */
/*    elaborate only: false */
/*    emit main: true */
/*    export header: None */
/*    exn history: false */
/*    generated output format: executable */
/*    gc check: Limit */
/*    indentation: 3 */
/*    inlineIntoMain: true */
/*    inlineLeafA: {loops = true, repeat = true, size = Some 20} */
/*    inlineLeafB: {loops = true, repeat = true, size = Some 40} */
/*    inlineNonRec: {small = 60, product = 320} */
/*    input file: args-create */
/*    keep AST: false */
/*    keep CoreML: false */
/*    keep def use: true */
/*    keep dot: false */
/*    keep Machine: false */
/*    keep passes: [] */
/*    keep RSSA: false */
/*    keep SSA: false */
/*    keep SSA2: false */
/*    keep SXML: false */
/*    keep XML: false */
/*    extra_: false */
/*    lib dir: /home/oem/pickle/take2/X86/build/lib */
/*    lib target dir: /home/oem/pickle/take2/X86/build/lib/targets/sparc-rtems4.11 */
/*    loop passes: 1 */
/*    mark cards: true */
/*    max function size: 10000 */
/*    mlb path vars: [{var = MLTON_ROOT, path = $(LIB_MLTON_DIR)/sml}, {var = SML_LIB, path = $(LIB_MLTON_DIR)/sml}] */
/*    native commented: 0 */
/*    native live stack: false */
/*    native optimize: 1 */
/*    native move hoist: true */
/*    native copy prop: true */
/*    native copy prop cutoff: 1000 */
/*    native cutoff: 100 */
/*    native live transfer: 8 */
/*    native shuffle: true */
/*    native ieee fp: false */
/*    native split: Some 20000 */
/*    optimizationPasses: [<ssa2::default>, <ssa::default>, <sxml::default>, <xml::default>] */
/*    polyvariance: Some {hofo = true, rounds = 2, small = 30, product = 300} */
/*    prefer abs paths: false */
/*    prof passes: [] */
/*    profile: None */
/*    profile branch: false */
/*    profile C: [] */
/*    profile IL: ProfileSource */
/*    profile include/exclude: [(Seq [Star [.], Or [Seq [Seq [[$], [(], [S], [M], [L], [_], [L], [I], [B], [)]]]], Star [.]], false)] */
/*    profile raise: false */
/*    profile stack: false */
/*    profile val: false */
/*    show basis: None */
/*    show def-use: None */
/*    show types: true */
/*    target: sparc-rtems4.11 */
/*    target arch: Sparc */
/*    target OS: Rtems */
/*    type check: false */
/*    verbosity: Silent */
/*    warn unrecognized annotation: true */
/*    warn deprecated features: true */
/*    zone cut depth: 100 */
#define _ISOC99_SOURCE
#include <c-main.h>

PRIVATE struct GC_state gcState;
PRIVATE CPointer globalCPointer [4];
PRIVATE CPointer CReturnQ;
PRIVATE Int8 globalInt8 [0];
PRIVATE Int8 CReturnI8;
PRIVATE Int16 globalInt16 [0];
PRIVATE Int16 CReturnI16;
PRIVATE Int32 globalInt32 [0];
PRIVATE Int32 CReturnI32;
PRIVATE Int64 globalInt64 [0];
PRIVATE Int64 CReturnI64;
PRIVATE Objptr globalObjptr [349];
PRIVATE Objptr CReturnP;
PRIVATE Real32 globalReal32 [0];
PRIVATE Real32 CReturnR32;
PRIVATE Real64 globalReal64 [0];
PRIVATE Real64 CReturnR64;
PRIVATE Word8 globalWord8 [0];
PRIVATE Word8 CReturnW8;
PRIVATE Word16 globalWord16 [0];
PRIVATE Word16 CReturnW16;
PRIVATE Word32 globalWord32 [3];
PRIVATE Word32 CReturnW32;
PRIVATE Word64 globalWord64 [0];
PRIVATE Word64 CReturnW64;
PRIVATE Pointer globalObjptrNonRoot [1];
PRIVATE Pointer MLton_FFI_opArgsResPtr;
static int saveGlobals (FILE *f) {
	SaveArray (globalCPointer, f);
	SaveArray (globalInt8, f);
	SaveArray (globalInt16, f);
	SaveArray (globalInt32, f);
	SaveArray (globalInt64, f);
	SaveArray (globalObjptr, f);
	SaveArray (globalReal32, f);
	SaveArray (globalReal64, f);
	SaveArray (globalWord8, f);
	SaveArray (globalWord16, f);
	SaveArray (globalWord32, f);
	SaveArray (globalWord64, f);
	return 0;
}
static int loadGlobals (FILE *f) {
	LoadArray (globalCPointer, f);
	LoadArray (globalInt8, f);
	LoadArray (globalInt16, f);
	LoadArray (globalInt32, f);
	LoadArray (globalInt64, f);
	LoadArray (globalObjptr, f);
	LoadArray (globalReal32, f);
	LoadArray (globalReal64, f);
	LoadArray (globalWord8, f);
	LoadArray (globalWord16, f);
	LoadArray (globalWord32, f);
	LoadArray (globalWord64, f);
	return 0;
}
BeginVectorInits
VectorInitElem (1, 262, 5, (pointer)"nostr")
VectorInitElem (1, 212, 12, (pointer)"addrnotavail")
VectorInitElem (1, 256, 6, (pointer)"nolink")
VectorInitElem (1, 347, 2, (pointer)" [")
VectorInitElem (1, 217, 6, (pointer)"badmsg")
VectorInitElem (1, 344, 4, (pointer)"Io: ")
VectorInitElem (1, 244, 8, (pointer)"multihop")
VectorInitElem (1, 210, 5, (pointer)"acces")
VectorInitElem (1, 259, 10, (pointer)"noprotoopt")
VectorInitElem (1, 231, 11, (pointer)"hostunreach")
VectorInitElem (1, 222, 11, (pointer)"connrefused")
VectorInitElem (1, 303, 8, (pointer)"bah \\bar")
VectorInitElem (1, 243, 7, (pointer)"msgsize")
VectorInitElem (1, 332, 6, (pointer)"FAIL: ")
VectorInitElem (1, 296, 1, (pointer)"\n")
VectorInitElem (1, 232, 4, (pointer)"idrm")
VectorInitElem (1, 214, 5, (pointer)"again")
VectorInitElem (1, 345, 6, (pointer)"Fail: ")
VectorInitElem (1, 304, 4, (pointer)"\\bah")
VectorInitElem (1, 310, 8, (pointer)"evil\narg")
VectorInitElem (1, 306, 8, (pointer)"evil\farg")
VectorInitElem (1, 235, 4, (pointer)"intr")
VectorInitElem (1, 316, 19, (pointer)"Posix.Process.exece")
VectorInitElem (1, 329, 21, (pointer)"Wrong argument count\n")
VectorInitElem (1, 266, 8, (pointer)"notempty")
VectorInitElem (1, 260, 5, (pointer)"nospc")
VectorInitElem (1, 240, 4, (pointer)"loop")
VectorInitElem (1, 335, 1, (pointer)"\000")
VectorInitElem (1, 289, 1, (pointer)"]")
VectorInitElem (1, 227, 5, (pointer)"dquot")
VectorInitElem (1, 211, 9, (pointer)"addrinuse")
VectorInitElem (1, 287, 10, (pointer)"wouldblock")
VectorInitElem (1, 286, 6, (pointer)"txtbsy")
VectorInitElem (1, 300, 6, (pointer)"ba h\\\\")
VectorInitElem (1, 263, 5, (pointer)"nosys")
VectorInitElem (1, 270, 4, (pointer)"nxio")
VectorInitElem (1, 333, 1, (pointer)":")
VectorInitElem (1, 281, 4, (pointer)"srch")
VectorInitElem (1, 279, 4, (pointer)"rofs")
VectorInitElem (1, 234, 10, (pointer)"inprogress")
VectorInitElem (1, 247, 8, (pointer)"netreset")
VectorInitElem (1, 228, 5, (pointer)"exist")
VectorInitElem (1, 339, 16, (pointer)"0123456789ABCDEF")
VectorInitElem (1, 252, 5, (pointer)"nodev")
VectorInitElem (1, 268, 6, (pointer)"notsup")
VectorInitElem (1, 221, 11, (pointer)"connaborted")
VectorInitElem (1, 272, 8, (pointer)"overflow")
VectorInitElem (1, 276, 14, (pointer)"protonosupport")
VectorInitElem (1, 236, 5, (pointer)"inval")
VectorInitElem (1, 218, 4, (pointer)"busy")
VectorInitElem (1, 246, 7, (pointer)"netdown")
VectorInitElem (1, 319, 5, (pointer)"Fail ")
VectorInitElem (1, 324, 8, (pointer)"Overflow")
VectorInitElem (1, 282, 5, (pointer)"stale")
VectorInitElem (1, 280, 5, (pointer)"spipe")
VectorInitElem (1, 226, 3, (pointer)"dom")
VectorInitElem (1, 251, 6, (pointer)"nodata")
VectorInitElem (1, 346, 9, (pointer)"<UNKNOWN>")
VectorInitElem (1, 254, 6, (pointer)"noexec")
VectorInitElem (1, 311, 3, (pointer)"hi\"")
VectorInitElem (1, 213, 11, (pointer)"afnosupport")
VectorInitElem (1, 233, 5, (pointer)"ilseq")
VectorInitElem (1, 330, 35, (pointer)"Top-level suffix raised exception.\n")
VectorInitElem (1, 275, 5, (pointer)"proto")
VectorInitElem (1, 337, 8, (pointer)"flushOut")
VectorInitElem (1, 291, 16, (pointer)"Thread.atomicEnd")
VectorInitElem (1, 340, 16, (pointer)"MLton.Exit.exit(")
VectorInitElem (1, 321, 4, (pointer)"Fail")
VectorInitElem (1, 242, 5, (pointer)"mlink")
VectorInitElem (1, 308, 8, (pointer)"evil arg")
VectorInitElem (1, 230, 4, (pointer)"fbig")
VectorInitElem (1, 264, 7, (pointer)"notconn")
VectorInitElem (1, 293, 32, (pointer)"exit must have 0 <= status < 256")
VectorInitElem (1, 334, 4, (pointer)"OK!\n")
VectorInitElem (1, 328, 8, (pointer)"<stdout>")
VectorInitElem (1, 314, 10, (pointer)"c:\\foo.bah")
VectorInitElem (1, 290, 21, (pointer)"NullString.fromString")
VectorInitElem (1, 315, 8, (pointer)"\"hello\\\"")
VectorInitElem (1, 348, 8, (pointer)"SysErr: ")
VectorInitElem (1, 261, 4, (pointer)"nosr")
VectorInitElem (1, 307, 8, (pointer)"evil\rarg")
VectorInitElem (1, 302, 7, (pointer)"bah\\bar")
VectorInitElem (1, 342, 14, (pointer)"\" failed with ")
VectorInitElem (1, 274, 4, (pointer)"pipe")
VectorInitElem (1, 299, 10, (pointer)"holy\"smoke")
VectorInitElem (1, 341, 5, (pointer)"Fail8")
VectorInitElem (1, 322, 6, (pointer)"SysErr")
VectorInitElem (1, 317, 28, (pointer)"control shouldn\'t reach here")
VectorInitElem (1, 325, 4, (pointer)"Size")
VectorInitElem (1, 224, 6, (pointer)"deadlk")
VectorInitElem (1, 269, 5, (pointer)"notty")
VectorInitElem (1, 238, 6, (pointer)"isconn")
VectorInitElem (1, 220, 5, (pointer)"child")
VectorInitElem (1, 278, 5, (pointer)"range")
VectorInitElem (1, 312, 3, (pointer)"hi\\")
VectorInitElem (1, 249, 5, (pointer)"nfile")
VectorInitElem (1, 245, 11, (pointer)"nametoolong")
VectorInitElem (1, 331, 36, (pointer)"Top-level handler raised exception.\n")
VectorInitElem (1, 215, 7, (pointer)"already")
VectorInitElem (1, 277, 9, (pointer)"prototype")
VectorInitElem (1, 229, 5, (pointer)"fault")
VectorInitElem (1, 338, 6, (pointer)"output")
VectorInitElem (1, 301, 5, (pointer)"bah\\\\")
VectorInitElem (1, 257, 5, (pointer)"nomem")
VectorInitElem (1, 219, 8, (pointer)"canceled")
VectorInitElem (1, 255, 5, (pointer)"nolck")
VectorInitElem (1, 258, 5, (pointer)"nomsg")
VectorInitElem (1, 343, 2, (pointer)" \"")
VectorInitElem (1, 305, 5, (pointer)"\"bar\\")
VectorInitElem (1, 327, 12, (pointer)"ClosedStream")
VectorInitElem (1, 295, 21, (pointer)"MLton.Exit.wrapSuffix")
VectorInitElem (1, 223, 9, (pointer)"connreset")
VectorInitElem (1, 241, 5, (pointer)"mfile")
VectorInitElem (1, 253, 5, (pointer)"noent")
VectorInitElem (1, 225, 11, (pointer)"destaddrreq")
VectorInitElem (1, 320, 36, (pointer)"unhandled exception in Basis Library")
VectorInitElem (1, 336, 13, (pointer)"Unknown error")
VectorInitElem (1, 284, 8, (pointer)"timedout")
VectorInitElem (1, 216, 4, (pointer)"badf")
VectorInitElem (1, 309, 8, (pointer)"evil\targ")
VectorInitElem (1, 297, 24, (pointer)"Posix.Process.fromStatus")
VectorInitElem (1, 237, 2, (pointer)"io")
VectorInitElem (1, 273, 4, (pointer)"perm")
VectorInitElem (1, 292, 13, (pointer)"partial write")
VectorInitElem (1, 298, 11, (pointer)"holy \"smoke")
VectorInitElem (1, 288, 4, (pointer)"xdev")
VectorInitElem (1, 313, 0, (pointer)"")
VectorInitElem (1, 250, 6, (pointer)"nobufs")
VectorInitElem (1, 323, 2, (pointer)"Io")
VectorInitElem (1, 326, 9, (pointer)"Subscript")
VectorInitElem (1, 318, 21, (pointer)"unhandled exception: ")
VectorInitElem (1, 265, 6, (pointer)"notdir")
VectorInitElem (1, 248, 10, (pointer)"netunreach")
VectorInitElem (1, 294, 3, (pointer)"): ")
VectorInitElem (1, 285, 6, (pointer)"toobig")
VectorInitElem (1, 283, 4, (pointer)"time")
VectorInitElem (1, 271, 9, (pointer)"opnotsupp")
VectorInitElem (1, 267, 7, (pointer)"notsock")
VectorInitElem (1, 239, 5, (pointer)"isdir")
EndVectorInits
static void real_Init() {
}
static uint16_t frameOffsets0[] = {0};
static uint16_t frameOffsets1[] = {4,8,12,16,20};
static uint16_t frameOffsets2[] = {5,8,12,16,20,28};
static uint16_t frameOffsets3[] = {10,8,12,16,20,24,28,32,36,44,52};
static uint16_t frameOffsets4[] = {7,8,16,20,24,28,32,36};
static uint16_t frameOffsets5[] = {4,12,20,24,40};
static uint16_t frameOffsets6[] = {5,12,20,24,28,40};
static uint16_t frameOffsets7[] = {2,20,40};
static uint16_t frameOffsets8[] = {1,8};
static uint16_t frameOffsets9[] = {2,8,12};
static uint16_t frameOffsets10[] = {6,8,12,16,20,24,40};
static uint16_t frameOffsets11[] = {5,8,12,20,24,40};
static uint16_t frameOffsets12[] = {5,8,20,24,28,40};
static uint16_t frameOffsets13[] = {4,20,24,28,40};
static uint16_t frameOffsets14[] = {6,8,12,20,24,28,40};
static uint16_t frameOffsets15[] = {7,8,12,16,20,24,28,40};
static uint16_t frameOffsets16[] = {4,8,16,20,40};
static uint16_t frameOffsets17[] = {3,16,20,40};
static uint16_t frameOffsets18[] = {3,8,20,40};
static uint16_t frameOffsets19[] = {6,12,16,20,24,32,40};
static uint16_t frameOffsets20[] = {5,16,20,28,32,40};
static uint16_t frameOffsets21[] = {7,16,20,24,28,32,36,40};
static uint16_t frameOffsets22[] = {7,12,16,20,24,32,36,40};
static uint16_t frameOffsets23[] = {7,8,12,20,24,28,32,40};
static uint16_t frameOffsets24[] = {6,8,20,24,28,32,40};
static uint16_t frameOffsets25[] = {3,20,24,40};
static uint16_t frameOffsets26[] = {4,8,12,20,40};
static uint16_t frameOffsets27[] = {6,12,16,20,24,28,40};
static uint16_t frameOffsets28[] = {6,12,20,24,28,32,40};
static uint16_t frameOffsets29[] = {8,8,12,16,20,24,28,32,36};
static uint16_t frameOffsets30[] = {9,8,12,16,20,24,28,32,36,40};
static uint16_t frameOffsets31[] = {1,20};
static uint16_t frameOffsets32[] = {8,8,12,20,24,28,32,36,44};
static uint16_t frameOffsets33[] = {9,8,12,16,20,24,28,32,36,44};
static uint16_t frameOffsets34[] = {7,8,12,16,20,24,28,32};
static uint16_t frameOffsets35[] = {6,8,12,16,20,24,28};
static uint16_t frameOffsets36[] = {5,8,12,16,20,24};
static uint16_t frameOffsets37[] = {3,8,12,16};
static uint16_t frameOffsets38[] = {4,0,4,16,20};
static uint16_t frameOffsets39[] = {5,0,4,16,28,32};
static uint16_t frameOffsets40[] = {2,16,20};
static uint16_t frameOffsets41[] = {1,16};
static uint16_t frameOffsets42[] = {1,4};
static uint16_t frameOffsets43[] = {2,4,24};
static uint16_t frameOffsets44[] = {5,0,4,16,24,28};
static uint16_t frameOffsets45[] = {2,0,4};
static uint16_t frameOffsets46[] = {3,8,20,28};
static uint16_t frameOffsets47[] = {1,12};
static uint16_t frameOffsets48[] = {1,0};
static uint16_t frameOffsets49[] = {2,16,24};
static uint16_t frameOffsets50[] = {2,16,36};
static uint16_t frameOffsets51[] = {2,4,16};
static uint16_t frameOffsets52[] = {2,4,8};
static uint16_t frameOffsets53[] = {3,0,4,8};
static uint16_t frameOffsets54[] = {3,4,8,12};
static struct GC_frameLayout frameLayouts[] = {
	{C_FRAME, frameOffsets0, 8},
	{ML_FRAME, frameOffsets0, 8},
	{C_FRAME, frameOffsets1, 32},
	{C_FRAME, frameOffsets2, 40},
	{C_FRAME, frameOffsets3, 64},
	{C_FRAME, frameOffsets4, 48},
	{C_FRAME, frameOffsets5, 48},
	{C_FRAME, frameOffsets6, 48},
	{ML_FRAME, frameOffsets7, 48},
	{ML_FRAME, frameOffsets0, 16},
	{ML_FRAME, frameOffsets0, 4},
	{ML_FRAME, frameOffsets8, 16},
	{C_FRAME, frameOffsets9, 24},
	{ML_FRAME, frameOffsets10, 48},
	{ML_FRAME, frameOffsets11, 48},
	{ML_FRAME, frameOffsets6, 48},
	{ML_FRAME, frameOffsets12, 48},
	{ML_FRAME, frameOffsets13, 48},
	{ML_FRAME, frameOffsets14, 48},
	{ML_FRAME, frameOffsets15, 48},
	{ML_FRAME, frameOffsets16, 48},
	{ML_FRAME, frameOffsets17, 48},
	{ML_FRAME, frameOffsets18, 48},
	{C_FRAME, frameOffsets17, 48},
	{C_FRAME, frameOffsets12, 48},
	{C_FRAME, frameOffsets10, 48},
	{C_FRAME, frameOffsets19, 48},
	{C_FRAME, frameOffsets20, 48},
	{ML_FRAME, frameOffsets21, 48},
	{ML_FRAME, frameOffsets22, 48},
	{C_FRAME, frameOffsets23, 48},
	{C_FRAME, frameOffsets24, 56},
	{C_FRAME, frameOffsets15, 48},
	{C_FRAME, frameOffsets14, 48},
	{C_FRAME, frameOffsets18, 48},
	{C_FRAME, frameOffsets7, 48},
	{C_FRAME, frameOffsets25, 48},
	{C_FRAME, frameOffsets13, 48},
	{C_FRAME, frameOffsets26, 48},
	{C_FRAME, frameOffsets27, 48},
	{C_FRAME, frameOffsets28, 56},
	{C_FRAME, frameOffsets29, 48},
	{C_FRAME, frameOffsets30, 48},
	{C_FRAME, frameOffsets0, 24},
	{C_FRAME, frameOffsets31, 32},
	{C_FRAME, frameOffsets0, 16},
	{C_FRAME, frameOffsets32, 56},
	{C_FRAME, frameOffsets33, 56},
	{C_FRAME, frameOffsets3, 72},
	{C_FRAME, frameOffsets34, 40},
	{C_FRAME, frameOffsets35, 40},
	{C_FRAME, frameOffsets36, 32},
	{C_FRAME, frameOffsets37, 24},
	{C_FRAME, frameOffsets1, 40},
	{ML_FRAME, frameOffsets38, 32},
	{ML_FRAME, frameOffsets39, 40},
	{C_FRAME, frameOffsets39, 48},
	{ML_FRAME, frameOffsets0, 12},
	{C_FRAME, frameOffsets40, 32},
	{ML_FRAME, frameOffsets41, 24},
	{C_FRAME, frameOffsets38, 32},
	{C_FRAME, frameOffsets42, 32},
	{C_FRAME, frameOffsets43, 48},
	{C_FRAME, frameOffsets44, 56},
	{C_FRAME, frameOffsets45, 24},
	{ML_FRAME, frameOffsets46, 40},
	{C_FRAME, frameOffsets46, 40},
	{C_FRAME, frameOffsets45, 16},
	{C_FRAME, frameOffsets47, 24},
	{C_FRAME, frameOffsets48, 16},
	{C_FRAME, frameOffsets49, 40},
	{ML_FRAME, frameOffsets0, 24},
	{C_FRAME, frameOffsets50, 48},
	{C_FRAME, frameOffsets41, 32},
	{ML_FRAME, frameOffsets51, 24},
	{C_FRAME, frameOffsets51, 24},
	{C_FRAME, frameOffsets42, 24},
	{C_FRAME, frameOffsets52, 24},
	{C_FRAME, frameOffsets8, 24},
	{C_FRAME, frameOffsets53, 16},
	{C_FRAME, frameOffsets48, 8},
	{ML_FRAME, frameOffsets52, 16},
	{C_FRAME, frameOffsets54, 24},
	{ML_FRAME, frameOffsets42, 16},
	{C_FRAME, frameOffsets52, 16},
	{C_FRAME, frameOffsets42, 16},
};
static struct GC_objectType objectTypes[] = {
	{ STACK_TAG, FALSE, 0, 0 },
	{ NORMAL_TAG, TRUE, 8, 1 },
	{ WEAK_TAG, FALSE, 12, 0 },
	{ ARRAY_TAG, FALSE, 1, 0 },
	{ ARRAY_TAG, FALSE, 4, 0 },
	{ ARRAY_TAG, FALSE, 2, 0 },
	{ ARRAY_TAG, FALSE, 8, 0 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ ARRAY_TAG, TRUE, 1, 0 },
	{ ARRAY_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, FALSE, 12, 0 },
	{ NORMAL_TAG, FALSE, 8, 1 },
	{ NORMAL_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, TRUE, 12, 0 },
	{ NORMAL_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, FALSE, 4, 4 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, FALSE, 12, 0 },
	{ ARRAY_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 0, 1 },
	{ NORMAL_TAG, FALSE, 0, 1 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, FALSE, 4, 0 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 8, 1 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 8, 1 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 2 },
	{ NORMAL_TAG, FALSE, 4, 0 },
	{ NORMAL_TAG, FALSE, 8, 1 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, FALSE, 8, 1 },
	{ NORMAL_TAG, FALSE, 12, 0 },
};
static uint32_t* sourceSeqs[] = {
};
static GC_sourceSeqIndex frameSources[] = {
};
static struct GC_sourceLabel sourceLabels[] = {
};
static char* sourceNames[] = {
};
static struct GC_source sources[] = {
};
static char* atMLtons[] = {
	"@MLton",
	"--",
};
DeclareChunk (0);
PRIVATE struct cont ( *nextChunks []) () = {	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
	Chunkp (0),
};
MLtonMain (8, 0xCA4D7FC4, 72, TRUE, PROFILE_NONE, FALSE, 0, 158)
#define CONFIGURE_APPLICATION_DOES_NOT_NEED_CLOCK_DRIVER
#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER
#define CONFIGURE_USE_DEVFS_AS_BASE_FILESYSTEM
#define CONFIGURE_RTEMS_INIT_TASKS_TABLE
#define CONFIGURE_MAXIMUM_TASKS 1
#define CONFIGURE_INIT
#include <rtems/confdefs.h>
rtems_task Init(rtems_task_argument ignored) {
char* a[0]; //MLton has to be fed some dummy argc and argv arguments
(MLton_main (0, a));
exit(0); //Might be unnecessary shrugascii
}
