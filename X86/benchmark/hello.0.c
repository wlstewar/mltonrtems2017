/* MLton g9e30096-dirty (built Sun Oct 22 00:19:29 EDT 2017 on oem-virtual-machine) */
/*   created this file on Sun Oct 22 16:21:40 2017. */
/* Do not edit this file. */
/* Flag settings:  */
/*    align: 4 */
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
/*    input file: hello */
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
/*    lib target dir: /home/oem/pickle/take2/X86/build/lib/targets/i386-rtems4.11 */
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
/*    target: i386-rtems4.11 */
/*    target arch: X86 */
/*    target OS: Rtems */
/*    type check: false */
/*    verbosity: Detail */
/*    warn unrecognized annotation: true */
/*    warn deprecated features: true */
/*    zone cut depth: 100 */
#define _ISOC99_SOURCE
#include <c-main.h>

PRIVATE struct GC_state gcState;
PRIVATE CPointer globalCPointer [0];
PRIVATE CPointer CReturnQ;
PRIVATE Int8 globalInt8 [0];
PRIVATE Int8 CReturnI8;
PRIVATE Int16 globalInt16 [0];
PRIVATE Int16 CReturnI16;
PRIVATE Int32 globalInt32 [0];
PRIVATE Int32 CReturnI32;
PRIVATE Int64 globalInt64 [0];
PRIVATE Int64 CReturnI64;
PRIVATE Objptr globalObjptr [291];
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
VectorInitElem (1, 180, 11, (pointer)"afnosupport")
VectorInitElem (1, 229, 5, (pointer)"nostr")
VectorInitElem (1, 179, 12, (pointer)"addrnotavail")
VectorInitElem (1, 200, 5, (pointer)"ilseq")
VectorInitElem (1, 223, 6, (pointer)"nolink")
VectorInitElem (1, 287, 2, (pointer)" [")
VectorInitElem (1, 184, 6, (pointer)"badmsg")
VectorInitElem (1, 277, 35, (pointer)"Top-level suffix raised exception.\n")
VectorInitElem (1, 242, 5, (pointer)"proto")
VectorInitElem (1, 278, 8, (pointer)"flushOut")
VectorInitElem (1, 257, 16, (pointer)"Thread.atomicEnd")
VectorInitElem (1, 284, 4, (pointer)"Io: ")
VectorInitElem (1, 211, 8, (pointer)"multihop")
VectorInitElem (1, 290, 16, (pointer)"MLton.Exit.exit(")
VectorInitElem (1, 267, 4, (pointer)"Fail")
VectorInitElem (1, 209, 5, (pointer)"mlink")
VectorInitElem (1, 177, 5, (pointer)"acces")
VectorInitElem (1, 226, 10, (pointer)"noprotoopt")
VectorInitElem (1, 198, 11, (pointer)"hostunreach")
VectorInitElem (1, 197, 4, (pointer)"fbig")
VectorInitElem (1, 231, 7, (pointer)"notconn")
VectorInitElem (1, 189, 11, (pointer)"connrefused")
VectorInitElem (1, 259, 32, (pointer)"exit must have 0 <= status < 256")
VectorInitElem (1, 274, 8, (pointer)"<stdout>")
VectorInitElem (1, 210, 7, (pointer)"msgsize")
VectorInitElem (1, 279, 13, (pointer)"hello world!\n")
VectorInitElem (1, 262, 1, (pointer)"\n")
VectorInitElem (1, 199, 4, (pointer)"idrm")
VectorInitElem (1, 181, 5, (pointer)"again")
VectorInitElem (1, 288, 8, (pointer)"SysErr: ")
VectorInitElem (1, 228, 4, (pointer)"nosr")
VectorInitElem (1, 285, 6, (pointer)"Fail: ")
VectorInitElem (1, 282, 14, (pointer)"\" failed with ")
VectorInitElem (1, 241, 4, (pointer)"pipe")
VectorInitElem (1, 281, 5, (pointer)"Fail8")
VectorInitElem (1, 263, 28, (pointer)"control shouldn\'t reach here")
VectorInitElem (1, 268, 6, (pointer)"SysErr")
VectorInitElem (1, 271, 4, (pointer)"Size")
VectorInitElem (1, 191, 6, (pointer)"deadlk")
VectorInitElem (1, 202, 4, (pointer)"intr")
VectorInitElem (1, 236, 5, (pointer)"notty")
VectorInitElem (1, 205, 6, (pointer)"isconn")
VectorInitElem (1, 187, 5, (pointer)"child")
VectorInitElem (1, 245, 5, (pointer)"range")
VectorInitElem (1, 233, 8, (pointer)"notempty")
VectorInitElem (1, 227, 5, (pointer)"nospc")
VectorInitElem (1, 207, 4, (pointer)"loop")
VectorInitElem (1, 216, 5, (pointer)"nfile")
VectorInitElem (1, 256, 1, (pointer)"]")
VectorInitElem (1, 212, 11, (pointer)"nametoolong")
VectorInitElem (1, 194, 5, (pointer)"dquot")
VectorInitElem (1, 276, 36, (pointer)"Top-level handler raised exception.\n")
VectorInitElem (1, 178, 9, (pointer)"addrinuse")
VectorInitElem (1, 182, 7, (pointer)"already")
VectorInitElem (1, 253, 6, (pointer)"txtbsy")
VectorInitElem (1, 254, 10, (pointer)"wouldblock")
VectorInitElem (1, 196, 5, (pointer)"fault")
VectorInitElem (1, 244, 9, (pointer)"prototype")
VectorInitElem (1, 275, 6, (pointer)"output")
VectorInitElem (1, 230, 5, (pointer)"nosys")
VectorInitElem (1, 237, 4, (pointer)"nxio")
VectorInitElem (1, 224, 5, (pointer)"nomem")
VectorInitElem (1, 186, 8, (pointer)"canceled")
VectorInitElem (1, 248, 4, (pointer)"srch")
VectorInitElem (1, 246, 4, (pointer)"rofs")
VectorInitElem (1, 222, 5, (pointer)"nolck")
VectorInitElem (1, 214, 8, (pointer)"netreset")
VectorInitElem (1, 201, 10, (pointer)"inprogress")
VectorInitElem (1, 195, 5, (pointer)"exist")
VectorInitElem (1, 225, 5, (pointer)"nomsg")
VectorInitElem (1, 283, 2, (pointer)" \"")
VectorInitElem (1, 289, 16, (pointer)"0123456789ABCDEF")
VectorInitElem (1, 273, 12, (pointer)"ClosedStream")
VectorInitElem (1, 261, 21, (pointer)"MLton.Exit.wrapSuffix")
VectorInitElem (1, 219, 5, (pointer)"nodev")
VectorInitElem (1, 235, 6, (pointer)"notsup")
VectorInitElem (1, 190, 9, (pointer)"connreset")
VectorInitElem (1, 188, 11, (pointer)"connaborted")
VectorInitElem (1, 239, 8, (pointer)"overflow")
VectorInitElem (1, 208, 5, (pointer)"mfile")
VectorInitElem (1, 220, 5, (pointer)"noent")
VectorInitElem (1, 243, 14, (pointer)"protonosupport")
VectorInitElem (1, 192, 11, (pointer)"destaddrreq")
VectorInitElem (1, 266, 36, (pointer)"unhandled exception in Basis Library")
VectorInitElem (1, 280, 13, (pointer)"Unknown error")
VectorInitElem (1, 251, 8, (pointer)"timedout")
VectorInitElem (1, 203, 5, (pointer)"inval")
VectorInitElem (1, 185, 4, (pointer)"busy")
VectorInitElem (1, 213, 7, (pointer)"netdown")
VectorInitElem (1, 265, 5, (pointer)"Fail ")
VectorInitElem (1, 183, 4, (pointer)"badf")
VectorInitElem (1, 270, 8, (pointer)"Overflow")
VectorInitElem (1, 204, 2, (pointer)"io")
VectorInitElem (1, 240, 4, (pointer)"perm")
VectorInitElem (1, 249, 5, (pointer)"stale")
VectorInitElem (1, 258, 13, (pointer)"partial write")
VectorInitElem (1, 247, 5, (pointer)"spipe")
VectorInitElem (1, 255, 4, (pointer)"xdev")
VectorInitElem (1, 193, 3, (pointer)"dom")
VectorInitElem (1, 217, 6, (pointer)"nobufs")
VectorInitElem (1, 269, 2, (pointer)"Io")
VectorInitElem (1, 272, 9, (pointer)"Subscript")
VectorInitElem (1, 218, 6, (pointer)"nodata")
VectorInitElem (1, 286, 9, (pointer)"<UNKNOWN>")
VectorInitElem (1, 221, 6, (pointer)"noexec")
VectorInitElem (1, 264, 21, (pointer)"unhandled exception: ")
VectorInitElem (1, 232, 6, (pointer)"notdir")
VectorInitElem (1, 215, 10, (pointer)"netunreach")
VectorInitElem (1, 260, 3, (pointer)"): ")
VectorInitElem (1, 252, 6, (pointer)"toobig")
VectorInitElem (1, 250, 4, (pointer)"time")
VectorInitElem (1, 238, 9, (pointer)"opnotsupp")
VectorInitElem (1, 234, 7, (pointer)"notsock")
VectorInitElem (1, 206, 5, (pointer)"isdir")
EndVectorInits
static void real_Init() {
}
static uint16_t frameOffsets0[] = {0};
static uint16_t frameOffsets1[] = {5,8,12,16,20,24};
static uint16_t frameOffsets2[] = {10,8,12,16,20,24,28,32,36,44,52};
static uint16_t frameOffsets3[] = {8,8,12,16,20,24,28,32,36};
static uint16_t frameOffsets4[] = {1,8};
static uint16_t frameOffsets5[] = {2,8,12};
static uint16_t frameOffsets6[] = {5,24,40,44,48,52};
static uint16_t frameOffsets7[] = {4,24,40,44,48};
static uint16_t frameOffsets8[] = {3,8,24,44};
static uint16_t frameOffsets9[] = {4,8,12,24,44};
static uint16_t frameOffsets10[] = {2,24,44};
static uint16_t frameOffsets11[] = {3,24,40,44};
static uint16_t frameOffsets12[] = {4,8,24,44,48};
static uint16_t frameOffsets13[] = {4,16,24,40,44};
static uint16_t frameOffsets14[] = {4,12,24,40,44};
static uint16_t frameOffsets15[] = {6,8,12,24,40,44,48};
static uint16_t frameOffsets16[] = {9,8,12,16,20,24,28,32,36,40};
static uint16_t frameOffsets17[] = {1,20};
static uint16_t frameOffsets18[] = {8,8,12,16,24,28,32,36,44};
static uint16_t frameOffsets19[] = {9,8,12,16,20,24,28,32,36,44};
static uint16_t frameOffsets20[] = {6,8,12,16,20,24,32};
static uint16_t frameOffsets21[] = {6,8,12,16,20,24,28};
static uint16_t frameOffsets22[] = {4,8,12,16,20};
static uint16_t frameOffsets23[] = {2,4,8};
static uint16_t frameOffsets24[] = {3,4,8,12};
static uint16_t frameOffsets25[] = {1,0};
static uint16_t frameOffsets26[] = {1,4};
static uint16_t frameOffsets27[] = {2,0,4};
static uint16_t frameOffsets28[] = {3,0,4,8};
static uint16_t frameOffsets29[] = {2,16,24};
static uint16_t frameOffsets30[] = {2,16,36};
static uint16_t frameOffsets31[] = {1,16};
static uint16_t frameOffsets32[] = {2,4,16};
static uint16_t frameOffsets33[] = {1,12};
static struct GC_frameLayout frameLayouts[] = {
	{C_FRAME, frameOffsets0, 4},
	{ML_FRAME, frameOffsets0, 4},
	{C_FRAME, frameOffsets1, 32},
	{C_FRAME, frameOffsets2, 60},
	{C_FRAME, frameOffsets3, 44},
	{ML_FRAME, frameOffsets4, 16},
	{ML_FRAME, frameOffsets0, 12},
	{C_FRAME, frameOffsets5, 20},
	{ML_FRAME, frameOffsets6, 60},
	{ML_FRAME, frameOffsets7, 56},
	{ML_FRAME, frameOffsets8, 52},
	{C_FRAME, frameOffsets9, 52},
	{ML_FRAME, frameOffsets10, 52},
	{C_FRAME, frameOffsets11, 52},
	{C_FRAME, frameOffsets12, 56},
	{C_FRAME, frameOffsets13, 52},
	{C_FRAME, frameOffsets14, 52},
	{C_FRAME, frameOffsets15, 56},
	{C_FRAME, frameOffsets16, 48},
	{C_FRAME, frameOffsets0, 24},
	{C_FRAME, frameOffsets17, 28},
	{C_FRAME, frameOffsets0, 16},
	{C_FRAME, frameOffsets18, 52},
	{C_FRAME, frameOffsets19, 56},
	{C_FRAME, frameOffsets2, 68},
	{C_FRAME, frameOffsets20, 40},
	{C_FRAME, frameOffsets21, 36},
	{C_FRAME, frameOffsets22, 28},
	{C_FRAME, frameOffsets1, 36},
	{C_FRAME, frameOffsets1, 40},
	{C_FRAME, frameOffsets0, 12},
	{ML_FRAME, frameOffsets23, 16},
	{C_FRAME, frameOffsets24, 20},
	{C_FRAME, frameOffsets25, 8},
	{ML_FRAME, frameOffsets26, 12},
	{C_FRAME, frameOffsets23, 16},
	{C_FRAME, frameOffsets26, 12},
	{C_FRAME, frameOffsets27, 12},
	{C_FRAME, frameOffsets23, 20},
	{C_FRAME, frameOffsets4, 20},
	{C_FRAME, frameOffsets28, 16},
	{C_FRAME, frameOffsets29, 40},
	{ML_FRAME, frameOffsets0, 20},
	{C_FRAME, frameOffsets30, 44},
	{C_FRAME, frameOffsets31, 28},
	{ML_FRAME, frameOffsets32, 24},
	{C_FRAME, frameOffsets32, 24},
	{C_FRAME, frameOffsets26, 20},
	{C_FRAME, frameOffsets33, 20},
	{C_FRAME, frameOffsets0, 8},
};
static struct GC_objectType objectTypes[] = {
	{ STACK_TAG, FALSE, 0, 0 },
	{ NORMAL_TAG, TRUE, 8, 1 },
	{ WEAK_TAG, FALSE, 8, 0 },
	{ ARRAY_TAG, FALSE, 1, 0 },
	{ ARRAY_TAG, FALSE, 4, 0 },
	{ ARRAY_TAG, FALSE, 2, 0 },
	{ ARRAY_TAG, FALSE, 8, 0 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, FALSE, 8, 0 },
	{ NORMAL_TAG, FALSE, 4, 1 },
	{ NORMAL_TAG, TRUE, 4, 0 },
	{ ARRAY_TAG, TRUE, 1, 0 },
	{ NORMAL_TAG, TRUE, 8, 0 },
	{ NORMAL_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, FALSE, 0, 4 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, FALSE, 12, 0 },
	{ ARRAY_TAG, TRUE, 4, 0 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, TRUE, 0, 1 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 1 },
	{ NORMAL_TAG, FALSE, 0, 1 },
	{ NORMAL_TAG, FALSE, 4, 1 },
	{ NORMAL_TAG, FALSE, 0, 3 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
	{ NORMAL_TAG, FALSE, 0, 2 },
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
};
MLtonMain (4, 0xC2446166, 68, TRUE, PROFILE_NONE, FALSE, 0, 98)
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
