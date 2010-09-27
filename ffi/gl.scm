;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: opengl.scm
;;
;; description: Opengl 1.1 ffi interface
;;
;; author: David St-Hilaire
;;
;; Adapted to BlackHole and modified by Álvaro Castro-Castilla
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import ../core/system-conditional)

(%compile-cond ("Linux" ("-w -I/usr/include/GL"
                         "-lGL -lGLU"))
               ("Darwin" ("-w -I/opt/local/include/"
                          "-L/opt/local/lib -lGL -lGLU")))

(c-declare "#include <gl.h>")

(c-define-type GLenum unsigned-int)
(c-define-type GLboolean unsigned-char)
(c-define-type GLbitfield unsigned-int)
(c-define-type GLvoid void)
(c-define-type GLbyte signed-char)
(c-define-type GLshort short)
(c-define-type GLint int)
(c-define-type GLubyte unsigned-char)
(c-define-type GLushort unsigned-short)
(c-define-type GLuint unsigned-int)
(c-define-type GLsizei int)
(c-define-type GLfloat float)
(c-define-type GLclampf float)
(c-define-type GLdouble double)
(c-define-type GLclampd double)

(c-define-type GLenum* (pointer GLenum))
(c-define-type GLboolean* (pointer GLboolean))
(c-define-type GLbitfield* (pointer GLbitfield))
(c-define-type GLvoid* (pointer GLvoid))
(c-define-type GLbyte* (pointer GLbyte))
(c-define-type GLshort* (pointer GLshort))
(c-define-type GLint* (pointer GLint))
(c-define-type GLubyte* (pointer GLubyte))
(c-define-type GLushort* (pointer GLushort))
(c-define-type GLuint* (pointer GLuint))
(c-define-type GLsizei* (pointer GLsizei))
(c-define-type GLfloat* (pointer GLfloat))
(c-define-type GLclampf* (pointer GLclampf))
(c-define-type GLdouble* (pointer GLdouble))
(c-define-type GLclampd* (pointer GLclampd))

(c-define-type GLvoid** (pointer GLvoid*))

; /* Boolean values* /
(define GL_FALSE ((c-lambda () int "___result = GL_FALSE;")))
(define GL_TRUE ((c-lambda () int "___result = GL_TRUE;")))

; /* Data types* /
(define GL_BYTE ((c-lambda () int "___result = GL_BYTE;")))
(define GL_UNSIGNED_BYTE ((c-lambda () int "___result = GL_UNSIGNED_BYTE;")))
(define GL_SHORT ((c-lambda () int "___result = GL_SHORT;")))
(define GL_UNSIGNED_SHORT ((c-lambda () int "___result = GL_UNSIGNED_SHORT;")))
(define GL_INT ((c-lambda () int "___result = GL_INT;")))
(define GL_UNSIGNED_INT ((c-lambda () int "___result = GL_UNSIGNED_INT;")))
(define GL_FLOAT ((c-lambda () int "___result = GL_FLOAT;")))
(define GL_2_BYTES ((c-lambda () int "___result = GL_2_BYTES;")))
(define GL_3_BYTES ((c-lambda () int "___result = GL_3_BYTES;")))
(define GL_4_BYTES ((c-lambda () int "___result = GL_4_BYTES;")))
(define GL_DOUBLE ((c-lambda () int "___result = GL_DOUBLE;")))

; /* Primitives* /
(define GL_POINTS ((c-lambda () int "___result = GL_POINTS;")))
(define GL_LINES ((c-lambda () int "___result = GL_LINES;")))
(define GL_LINE_LOOP ((c-lambda () int "___result = GL_LINE_LOOP;")))				
(define GL_LINE_STRIP ((c-lambda () int "___result = GL_LINE_STRIP;")))
(define GL_TRIANGLES ((c-lambda () int "___result = GL_TRIANGLES;")))
(define GL_TRIANGLE_STRIP ((c-lambda () int "___result = GL_TRIANGLE_STRIP;")))
(define GL_TRIANGLE_FAN ((c-lambda () int "___result = GL_TRIANGLE_FAN;")))
(define GL_QUADS ((c-lambda () int "___result = GL_QUADS;")))
(define GL_QUAD_STRIP ((c-lambda () int "___result = GL_QUAD_STRIP;")))
(define GL_POLYGON ((c-lambda () int "___result = GL_POLYGON;")))
(define GL_VERTEX_ARRAY	#x8074)

; /* Arrays* /
(define GL_NORMAL_ARRAY ((c-lambda () int "___result = GL_NORMAL_ARRAY;")))
(define GL_COLOR_ARRAY ((c-lambda () int "___result = GL_COLOR_ARRAY;")))
(define GL_INDEX_ARRAY ((c-lambda () int "___result = GL_INDEX_ARRAY;")))
(define GL_TEXTURE_COORD_ARRAY ((c-lambda () int "___result = GL_TEXTURE_COORD_ARRAY;")))
(define GL_EDGE_FLAG_ARRAY ((c-lambda () int "___result = GL_EDGE_FLAG_ARRAY;")))
(define GL_VERTEX_ARRAY_SIZE ((c-lambda () int "___result = GL_VERTEX_ARRAY_SIZE;")))
(define GL_VERTEX_ARRAY_TYPE ((c-lambda () int "___result = GL_VERTEX_ARRAY_TYPE;")))
(define GL_VERTEX_ARRAY_STRIDE ((c-lambda () int "___result = GL_VERTEX_ARRAY_STRIDE;")))
(define GL_NORMAL_ARRAY_TYPE ((c-lambda () int "___result = GL_NORMAL_ARRAY_TYPE;")))
(define GL_NORMAL_ARRAY_STRIDE ((c-lambda () int "___result = GL_NORMAL_ARRAY_STRIDE;")))
(define GL_COLOR_ARRAY_SIZE ((c-lambda () int "___result = GL_COLOR_ARRAY_SIZE;")))
(define GL_COLOR_ARRAY_TYPE ((c-lambda () int "___result = GL_COLOR_ARRAY_TYPE;")))
(define GL_COLOR_ARRAY_STRIDE ((c-lambda () int "___result = GL_COLOR_ARRAY_STRIDE;")))
(define GL_INDEX_ARRAY_TYPE ((c-lambda () int "___result = GL_INDEX_ARRAY_TYPE;")))
(define GL_INDEX_ARRAY_STRIDE ((c-lambda () int "___result = GL_INDEX_ARRAY_STRIDE;")))
(define GL_TEXTURE_COORD_ARRAY_SIZE ((c-lambda () int "___result = GL_TEXTURE_COORD_ARRAY_SIZE;")))
(define GL_TEXTURE_COORD_ARRAY_TYPE ((c-lambda () int "___result = GL_TEXTURE_COORD_ARRAY_TYPE;")))
(define GL_TEXTURE_COORD_ARRAY_STRIDE ((c-lambda () int "___result = GL_TEXTURE_COORD_ARRAY_STRIDE;")))
(define GL_EDGE_FLAG_ARRAY_STRIDE ((c-lambda () int "___result = GL_EDGE_FLAG_ARRAY_STRIDE;")))
(define GL_VERTEX_ARRAY_POINTER ((c-lambda () int "___result = GL_VERTEX_ARRAY_POINTER;")))
(define GL_NORMAL_ARRAY_POINTER ((c-lambda () int "___result = GL_NORMAL_ARRAY_POINTER;")))
(define GL_COLOR_ARRAY_POINTER ((c-lambda () int "___result = GL_COLOR_ARRAY_POINTER;")))
(define GL_INDEX_ARRAY_POINTER ((c-lambda () int "___result = GL_INDEX_ARRAY_POINTER;")))
(define GL_TEXTURE_COORD_ARRAY_POINTER ((c-lambda () int "___result = GL_TEXTURE_COORD_ARRAY_POINTER;")))
(define GL_EDGE_FLAG_ARRAY_POINTER ((c-lambda () int "___result = GL_EDGE_FLAG_ARRAY_POINTER;")))
(define GL_V2F ((c-lambda () int "___result = GL_V2F;")))
(define GL_V3F ((c-lambda () int "___result = GL_V3F;")))
(define GL_C4UB_V2F ((c-lambda () int "___result = GL_C4UB_V2F;")))
(define GL_C4UB_V3F ((c-lambda () int "___result = GL_C4UB_V3F;")))
(define GL_C3F_V3F ((c-lambda () int "___result = GL_C3F_V3F;")))
(define GL_N3F_V3F ((c-lambda () int "___result = GL_N3F_V3F;")))(define GL_C4F_N3F_V3F ((c-lambda () int "___result = GL_C4F_N3F_V3F;")))
(define GL_T2F_V3F ((c-lambda () int "___result = GL_T2F_V3F;")))
(define GL_T4F_V4F ((c-lambda () int "___result = GL_T4F_V4F;")))
(define GL_T2F_C4UB_V3F ((c-lambda () int "___result = GL_T2F_C4UB_V3F;")))
(define GL_T2F_C3F_V3F ((c-lambda () int "___result = GL_T2F_C3F_V3F;")))
(define GL_T2F_N3F_V3F ((c-lambda () int "___result = GL_T2F_N3F_V3F;")))
(define GL_T2F_C4F_N3F_V3F ((c-lambda () int "___result = GL_T2F_C4F_N3F_V3F;")))
(define GL_T4F_C4F_N3F_V4F ((c-lambda () int "___result = GL_T4F_C4F_N3F_V4F;")))

; /* Matrix Mode* /
(define GL_MATRIX_MODE ((c-lambda () int "___result = GL_MATRIX_MODE;")))
(define GL_MODELVIEW ((c-lambda () int "___result = GL_MODELVIEW;")))
(define GL_PROJECTION ((c-lambda () int "___result = GL_PROJECTION;")))
(define GL_TEXTURE ((c-lambda () int "___result = GL_TEXTURE;")))

; /* Points* /
(define GL_POINT_SMOOTH ((c-lambda () int "___result = GL_POINT_SMOOTH;")))
(define GL_POINT_SIZE ((c-lambda () int "___result = GL_POINT_SIZE;")))
(define GL_POINT_SIZE_GRANULARITY ((c-lambda () int "___result = GL_POINT_SIZE_GRANULARITY;")))
(define GL_POINT_SIZE_RANGE ((c-lambda () int "___result = GL_POINT_SIZE_RANGE;")))

; /* Lines* /
(define GL_LINE_SMOOTH ((c-lambda () int "___result = GL_LINE_SMOOTH;")))
(define GL_LINE_STIPPLE ((c-lambda () int "___result = GL_LINE_STIPPLE;")))
(define GL_LINE_STIPPLE_PATTERN ((c-lambda () int "___result = GL_LINE_STIPPLE_PATTERN;")))
(define GL_LINE_STIPPLE_REPEAT ((c-lambda () int "___result = GL_LINE_STIPPLE_REPEAT;")))
(define GL_LINE_WIDTH ((c-lambda () int "___result = GL_LINE_WIDTH;")))
(define GL_LINE_WIDTH_GRANULARITY ((c-lambda () int "___result = GL_LINE_WIDTH_GRANULARITY;")))
(define GL_LINE_WIDTH_RANGE ((c-lambda () int "___result = GL_LINE_WIDTH_RANGE;")))

; /* Polygons* /
(define GL_POINT ((c-lambda () int "___result = GL_POINT;")))
(define GL_LINE ((c-lambda () int "___result = GL_LINE;")))
(define GL_FILL ((c-lambda () int "___result = GL_FILL;")))
(define GL_CW ((c-lambda () int "___result = GL_CW;")))
(define GL_CCW ((c-lambda () int "___result = GL_CCW;")))
(define GL_FRONT ((c-lambda () int "___result = GL_FRONT;")))
(define GL_BACK ((c-lambda () int "___result = GL_BACK;")))
(define GL_POLYGON_MODE ((c-lambda () int "___result = GL_POLYGON_MODE;")))
(define GL_POLYGON_SMOOTH ((c-lambda () int "___result = GL_POLYGON_SMOOTH;")))
(define GL_POLYGON_STIPPLE ((c-lambda () int "___result = GL_POLYGON_STIPPLE;")))
(define GL_EDGE_FLAG ((c-lambda () int "___result = GL_EDGE_FLAG;")))
(define GL_CULL_FACE ((c-lambda () int "___result = GL_CULL_FACE;")))
(define GL_CULL_FACE_MODE ((c-lambda () int "___result = GL_CULL_FACE_MODE;")))
(define GL_FRONT_FACE ((c-lambda () int "___result = GL_FRONT_FACE;")))(define GL_POLYGON_OFFSET_FACTOR ((c-lambda () int "___result = GL_POLYGON_OFFSET_FACTOR;")))
(define GL_POLYGON_OFFSET_UNITS ((c-lambda () int "___result = GL_POLYGON_OFFSET_UNITS;")))
(define GL_POLYGON_OFFSET_POINT ((c-lambda () int "___result = GL_POLYGON_OFFSET_POINT;")))
(define GL_POLYGON_OFFSET_LINE ((c-lambda () int "___result = GL_POLYGON_OFFSET_LINE;")))
(define GL_POLYGON_OFFSET_FILL ((c-lambda () int "___result = GL_POLYGON_OFFSET_FILL;")))

; /* Display Lists* /
(define GL_COMPILE ((c-lambda () int "___result = GL_COMPILE;")))
(define GL_COMPILE_AND_EXECUTE ((c-lambda () int "___result = GL_COMPILE_AND_EXECUTE;")))
(define GL_LIST_BASE ((c-lambda () int "___result = GL_LIST_BASE;")))
(define GL_LIST_INDEX ((c-lambda () int "___result = GL_LIST_INDEX;")))
(define GL_LIST_MODE ((c-lambda () int "___result = GL_LIST_MODE;")))

; /* Depth buffer* /
(define GL_NEVER ((c-lambda () int "___result = GL_NEVER;")))
(define GL_LESS ((c-lambda () int "___result = GL_LESS;")))
(define GL_EQUAL ((c-lambda () int "___result = GL_EQUAL;")))
(define GL_LEQUAL ((c-lambda () int "___result = GL_LEQUAL;")))
(define GL_GREATER ((c-lambda () int "___result = GL_GREATER;")))
(define GL_NOTEQUAL ((c-lambda () int "___result = GL_NOTEQUAL;")))
(define GL_GEQUAL ((c-lambda () int "___result = GL_GEQUAL;")))
(define GL_ALWAYS ((c-lambda () int "___result = GL_ALWAYS;")))
(define GL_DEPTH_TEST ((c-lambda () int "___result = GL_DEPTH_TEST;")))
(define GL_DEPTH_BITS ((c-lambda () int "___result = GL_DEPTH_BITS;")))
(define GL_DEPTH_CLEAR_VALUE ((c-lambda () int "___result = GL_DEPTH_CLEAR_VALUE;")))
(define GL_DEPTH_FUNC ((c-lambda () int "___result = GL_DEPTH_FUNC;")))
(define GL_DEPTH_RANGE ((c-lambda () int "___result = GL_DEPTH_RANGE;")))
(define GL_DEPTH_WRITEMASK ((c-lambda () int "___result = GL_DEPTH_WRITEMASK;")))
(define GL_DEPTH_COMPONENT ((c-lambda () int "___result = GL_DEPTH_COMPONENT;")))

; /* Lighting* /
(define GL_LIGHTING ((c-lambda () int "___result = GL_LIGHTING;")))
(define GL_LIGHT0 ((c-lambda () int "___result = GL_LIGHT0;")))
(define GL_LIGHT1 ((c-lambda () int "___result = GL_LIGHT1;")))
(define GL_LIGHT2 ((c-lambda () int "___result = GL_LIGHT2;")))
(define GL_LIGHT3 ((c-lambda () int "___result = GL_LIGHT3;")))
(define GL_LIGHT4 ((c-lambda () int "___result = GL_LIGHT4;")))
(define GL_LIGHT5 ((c-lambda () int "___result = GL_LIGHT5;")))
(define GL_LIGHT6 ((c-lambda () int "___result = GL_LIGHT6;")))
(define GL_LIGHT7 ((c-lambda () int "___result = GL_LIGHT7;")))
(define GL_SPOT_EXPONENT ((c-lambda () int "___result = GL_SPOT_EXPONENT;")))
(define GL_SPOT_CUTOFF ((c-lambda () int "___result = GL_SPOT_CUTOFF;")))
(define GL_CONSTANT_ATTENUATION ((c-lambda () int "___result = GL_CONSTANT_ATTENUATION;")))
(define GL_LINEAR_ATTENUATION ((c-lambda () int "___result = GL_LINEAR_ATTENUATION;")))
(define GL_QUADRATIC_ATTENUATION ((c-lambda () int "___result = GL_QUADRATIC_ATTENUATION;")))
(define GL_AMBIENT ((c-lambda () int "___result = GL_AMBIENT;")))
(define GL_DIFFUSE ((c-lambda () int "___result = GL_DIFFUSE;")))
(define GL_SPECULAR ((c-lambda () int "___result = GL_SPECULAR;")))
(define GL_SHININESS ((c-lambda () int "___result = GL_SHININESS;")))
(define GL_EMISSION ((c-lambda () int "___result = GL_EMISSION;")))
(define GL_POSITION ((c-lambda () int "___result = GL_POSITION;")))
(define GL_SPOT_DIRECTION ((c-lambda () int "___result = GL_SPOT_DIRECTION;")))
(define GL_AMBIENT_AND_DIFFUSE ((c-lambda () int "___result = GL_AMBIENT_AND_DIFFUSE;")))
(define GL_COLOR_INDEXES ((c-lambda () int "___result = GL_COLOR_INDEXES;")))
(define GL_LIGHT_MODEL_TWO_SIDE ((c-lambda () int "___result = GL_LIGHT_MODEL_TWO_SIDE;")))
(define GL_LIGHT_MODEL_LOCAL_VIEWER ((c-lambda () int "___result = GL_LIGHT_MODEL_LOCAL_VIEWER;")))
(define GL_LIGHT_MODEL_AMBIENT ((c-lambda () int "___result = GL_LIGHT_MODEL_AMBIENT;")))
(define GL_FRONT_AND_BACK ((c-lambda () int "___result = GL_FRONT_AND_BACK;")))
(define GL_SHADE_MODEL ((c-lambda () int "___result = GL_SHADE_MODEL;")))
(define GL_FLAT ((c-lambda () int "___result = GL_FLAT;")))
(define GL_SMOOTH ((c-lambda () int "___result = GL_SMOOTH;")))
(define GL_COLOR_MATERIAL ((c-lambda () int "___result = GL_COLOR_MATERIAL;")))
(define GL_COLOR_MATERIAL_FACE ((c-lambda () int "___result = GL_COLOR_MATERIAL_FACE;")))
(define GL_COLOR_MATERIAL_PARAMETER ((c-lambda () int "___result = GL_COLOR_MATERIAL_PARAMETER;")))
(define GL_NORMALIZE ((c-lambda () int "___result = GL_NORMALIZE;")))

; /* User clipping planes* /
(define GL_CLIP_PLANE0 ((c-lambda () int "___result = GL_CLIP_PLANE0;")))
(define GL_CLIP_PLANE1 ((c-lambda () int "___result = GL_CLIP_PLANE1;")))
(define GL_CLIP_PLANE2 ((c-lambda () int "___result = GL_CLIP_PLANE2;")))
(define GL_CLIP_PLANE3 ((c-lambda () int "___result = GL_CLIP_PLANE3;")))
(define GL_CLIP_PLANE4 ((c-lambda () int "___result = GL_CLIP_PLANE4;")))
(define GL_CLIP_PLANE5 ((c-lambda () int "___result = GL_CLIP_PLANE5;")))

; /* Accumulation buffer* /
(define GL_ACCUM_RED_BITS ((c-lambda () int "___result = GL_ACCUM_RED_BITS;")))
(define GL_ACCUM_GREEN_BITS ((c-lambda () int "___result = GL_ACCUM_GREEN_BITS;")))
(define GL_ACCUM_BLUE_BITS ((c-lambda () int "___result = GL_ACCUM_BLUE_BITS;")))
(define GL_ACCUM_ALPHA_BITS ((c-lambda () int "___result = GL_ACCUM_ALPHA_BITS;")))
(define GL_ACCUM_CLEAR_VALUE ((c-lambda () int "___result = GL_ACCUM_CLEAR_VALUE;")))
(define GL_ACCUM ((c-lambda () int "___result = GL_ACCUM;")))
(define GL_ADD ((c-lambda () int "___result = GL_ADD;")))
(define GL_LOAD ((c-lambda () int "___result = GL_LOAD;")))
(define GL_MULT ((c-lambda () int "___result = GL_MULT;")))					
(define GL_RETURN ((c-lambda () int "___result = GL_RETURN;")))

; /* Alpha testing* /
(define GL_ALPHA_TEST ((c-lambda () int "___result = GL_ALPHA_TEST;")))
(define GL_ALPHA_TEST_REF ((c-lambda () int "___result = GL_ALPHA_TEST_REF;")))
(define GL_ALPHA_TEST_FUNC ((c-lambda () int "___result = GL_ALPHA_TEST_FUNC;")))

; /* Blending* /
(define GL_BLEND ((c-lambda () int "___result = GL_BLEND;")))
(define GL_BLEND_SRC ((c-lambda () int "___result = GL_BLEND_SRC;")))
(define GL_BLEND_DST ((c-lambda () int "___result = GL_BLEND_DST;")))
(define GL_ZERO ((c-lambda () int "___result = GL_ZERO;")))
(define GL_ONE ((c-lambda () int "___result = GL_ONE;")))
(define GL_SRC_COLOR ((c-lambda () int "___result = GL_SRC_COLOR;")))
(define GL_ONE_MINUS_SRC_COLOR ((c-lambda () int "___result = GL_ONE_MINUS_SRC_COLOR;")))
(define GL_SRC_ALPHA ((c-lambda () int "___result = GL_SRC_ALPHA;")))
(define GL_ONE_MINUS_SRC_ALPHA ((c-lambda () int "___result = GL_ONE_MINUS_SRC_ALPHA;")))
(define GL_DST_ALPHA ((c-lambda () int "___result = GL_DST_ALPHA;")))
(define GL_ONE_MINUS_DST_ALPHA ((c-lambda () int "___result = GL_ONE_MINUS_DST_ALPHA;")))
(define GL_DST_COLOR ((c-lambda () int "___result = GL_DST_COLOR;")))
(define GL_ONE_MINUS_DST_COLOR ((c-lambda () int "___result = GL_ONE_MINUS_DST_COLOR;")))			
(define GL_SRC_ALPHA_SATURATE ((c-lambda () int "___result = GL_SRC_ALPHA_SATURATE;")))

; /* Render Mode* /
(define GL_FEEDBACK ((c-lambda () int "___result = GL_FEEDBACK;")))
(define GL_RENDER ((c-lambda () int "___result = GL_RENDER;")))
(define GL_SELECT ((c-lambda () int "___result = GL_SELECT;")))

; /* Feedback* /
(define GL_2D ((c-lambda () int "___result = GL_2D;")))
(define GL_3D ((c-lambda () int "___result = GL_3D;")))
(define GL_3D_COLOR ((c-lambda () int "___result = GL_3D_COLOR;")))
(define GL_3D_COLOR_TEXTURE ((c-lambda () int "___result = GL_3D_COLOR_TEXTURE;")))
(define GL_4D_COLOR_TEXTURE ((c-lambda () int "___result = GL_4D_COLOR_TEXTURE;")))
(define GL_POINT_TOKEN ((c-lambda () int "___result = GL_POINT_TOKEN;")))
(define GL_LINE_TOKEN ((c-lambda () int "___result = GL_LINE_TOKEN;")))
(define GL_LINE_RESET_TOKEN ((c-lambda () int "___result = GL_LINE_RESET_TOKEN;")))
(define GL_POLYGON_TOKEN ((c-lambda () int "___result = GL_POLYGON_TOKEN;")))
(define GL_BITMAP_TOKEN ((c-lambda () int "___result = GL_BITMAP_TOKEN;")))
(define GL_DRAW_PIXEL_TOKEN ((c-lambda () int "___result = GL_DRAW_PIXEL_TOKEN;")))
(define GL_COPY_PIXEL_TOKEN ((c-lambda () int "___result = GL_COPY_PIXEL_TOKEN;")))
(define GL_PASS_THROUGH_TOKEN ((c-lambda () int "___result = GL_PASS_THROUGH_TOKEN;")))
(define GL_FEEDBACK_BUFFER_POINTER ((c-lambda () int "___result = GL_FEEDBACK_BUFFER_POINTER;")))
(define GL_FEEDBACK_BUFFER_SIZE ((c-lambda () int "___result = GL_FEEDBACK_BUFFER_SIZE;")))
(define GL_FEEDBACK_BUFFER_TYPE ((c-lambda () int "___result = GL_FEEDBACK_BUFFER_TYPE;")))

; /* Selection* /
(define GL_SELECTION_BUFFER_POINTER ((c-lambda () int "___result = GL_SELECTION_BUFFER_POINTER;")))
(define GL_SELECTION_BUFFER_SIZE ((c-lambda () int "___result = GL_SELECTION_BUFFER_SIZE;")))

; /* Fog* /
(define GL_FOG ((c-lambda () int "___result = GL_FOG;")))
(define GL_FOG_MODE ((c-lambda () int "___result = GL_FOG_MODE;")))
(define GL_FOG_DENSITY ((c-lambda () int "___result = GL_FOG_DENSITY;")))
(define GL_FOG_COLOR ((c-lambda () int "___result = GL_FOG_COLOR;")))
(define GL_FOG_INDEX ((c-lambda () int "___result = GL_FOG_INDEX;")))
(define GL_FOG_START ((c-lambda () int "___result = GL_FOG_START;")))
(define GL_FOG_END ((c-lambda () int "___result = GL_FOG_END;")))
(define GL_LINEAR ((c-lambda () int "___result = GL_LINEAR;")))
(define GL_EXP ((c-lambda () int "___result = GL_EXP;")))
(define GL_EXP2 ((c-lambda () int "___result = GL_EXP2;")))

; /* Logic Ops* /
(define GL_LOGIC_OP ((c-lambda () int "___result = GL_LOGIC_OP;")))
(define GL_INDEX_LOGIC_OP ((c-lambda () int "___result = GL_INDEX_LOGIC_OP;")))
(define GL_COLOR_LOGIC_OP ((c-lambda () int "___result = GL_COLOR_LOGIC_OP;")))
(define GL_LOGIC_OP_MODE ((c-lambda () int "___result = GL_LOGIC_OP_MODE;")))
(define GL_CLEAR ((c-lambda () int "___result = GL_CLEAR;")))
(define GL_SET ((c-lambda () int "___result = GL_SET;")))
(define GL_COPY ((c-lambda () int "___result = GL_COPY;")))
(define GL_COPY_INVERTED ((c-lambda () int "___result = GL_COPY_INVERTED;")))
(define GL_NOOP ((c-lambda () int "___result = GL_NOOP;")))
(define GL_INVERT ((c-lambda () int "___result = GL_INVERT;")))
(define GL_AND ((c-lambda () int "___result = GL_AND;")))
(define GL_NAND ((c-lambda () int "___result = GL_NAND;")))
(define GL_OR ((c-lambda () int "___result = GL_OR;")))
(define GL_NOR ((c-lambda () int "___result = GL_NOR;")))
(define GL_XOR ((c-lambda () int "___result = GL_XOR;")))
(define GL_EQUIV ((c-lambda () int "___result = GL_EQUIV;")))
(define GL_AND_REVERSE ((c-lambda () int "___result = GL_AND_REVERSE;")))
(define GL_AND_INVERTED ((c-lambda () int "___result = GL_AND_INVERTED;")))
(define GL_OR_REVERSE ((c-lambda () int "___result = GL_OR_REVERSE;")))
(define GL_OR_INVERTED ((c-lambda () int "___result = GL_OR_INVERTED;")))

; /* Stencil* /
(define GL_STENCIL_BITS ((c-lambda () int "___result = GL_STENCIL_BITS;")))
(define GL_STENCIL_TEST ((c-lambda () int "___result = GL_STENCIL_TEST;")))
(define GL_STENCIL_CLEAR_VALUE ((c-lambda () int "___result = GL_STENCIL_CLEAR_VALUE;")))
(define GL_STENCIL_FUNC ((c-lambda () int "___result = GL_STENCIL_FUNC;")))
(define GL_STENCIL_VALUE_MASK ((c-lambda () int "___result = GL_STENCIL_VALUE_MASK;")))
(define GL_STENCIL_FAIL ((c-lambda () int "___result = GL_STENCIL_FAIL;")))
(define GL_STENCIL_PASS_DEPTH_FAIL ((c-lambda () int "___result = GL_STENCIL_PASS_DEPTH_FAIL;")))
(define GL_STENCIL_PASS_DEPTH_PASS ((c-lambda () int "___result = GL_STENCIL_PASS_DEPTH_PASS;")))
(define GL_STENCIL_REF ((c-lambda () int "___result = GL_STENCIL_REF;")))
(define GL_STENCIL_WRITEMASK ((c-lambda () int "___result = GL_STENCIL_WRITEMASK;")))
(define GL_STENCIL_INDEX ((c-lambda () int "___result = GL_STENCIL_INDEX;")))
(define GL_KEEP ((c-lambda () int "___result = GL_KEEP;")))
(define GL_REPLACE ((c-lambda () int "___result = GL_REPLACE;")))
(define GL_INCR ((c-lambda () int "___result = GL_INCR;")))
(define GL_DECR ((c-lambda () int "___result = GL_DECR;")))

; /* Buffers, Pixel Drawing/Reading* /
(define GL_NONE ((c-lambda () int "___result = GL_NONE;")))
(define GL_LEFT ((c-lambda () int "___result = GL_LEFT;")))
(define GL_RIGHT ((c-lambda () int "___result = GL_RIGHT;")))
(define GL_FRONT_LEFT ((c-lambda () int "___result = GL_FRONT_LEFT;")))
(define GL_FRONT_RIGHT ((c-lambda () int "___result = GL_FRONT_RIGHT;")))
(define GL_BACK_LEFT ((c-lambda () int "___result = GL_BACK_LEFT;")))
(define GL_BACK_RIGHT ((c-lambda () int "___result = GL_BACK_RIGHT;")))
(define GL_AUX0 ((c-lambda () int "___result = GL_AUX0;")))
(define GL_AUX1 ((c-lambda () int "___result = GL_AUX1;")))
(define GL_AUX2 ((c-lambda () int "___result = GL_AUX2;")))
(define GL_AUX3 ((c-lambda () int "___result = GL_AUX3;")))
(define GL_COLOR_INDEX ((c-lambda () int "___result = GL_COLOR_INDEX;")))
(define GL_RED ((c-lambda () int "___result = GL_RED;")))
(define GL_GREEN ((c-lambda () int "___result = GL_GREEN;")))
(define GL_BLUE ((c-lambda () int "___result = GL_BLUE;")))
(define GL_ALPHA ((c-lambda () int "___result = GL_ALPHA;")))
(define GL_LUMINANCE ((c-lambda () int "___result = GL_LUMINANCE;")))
(define GL_LUMINANCE_ALPHA ((c-lambda () int "___result = GL_LUMINANCE_ALPHA;")))
(define GL_ALPHA_BITS ((c-lambda () int "___result = GL_ALPHA_BITS;")))
(define GL_RED_BITS ((c-lambda () int "___result = GL_RED_BITS;")))
(define GL_GREEN_BITS ((c-lambda () int "___result = GL_GREEN_BITS;")))
(define GL_BLUE_BITS ((c-lambda () int "___result = GL_BLUE_BITS;")))
(define GL_INDEX_BITS ((c-lambda () int "___result = GL_INDEX_BITS;")))
(define GL_SUBPIXEL_BITS ((c-lambda () int "___result = GL_SUBPIXEL_BITS;")))
(define GL_AUX_BUFFERS ((c-lambda () int "___result = GL_AUX_BUFFERS;")))
(define GL_READ_BUFFER ((c-lambda () int "___result = GL_READ_BUFFER;")))
(define GL_DRAW_BUFFER ((c-lambda () int "___result = GL_DRAW_BUFFER;")))
(define GL_DOUBLEBUFFER ((c-lambda () int "___result = GL_DOUBLEBUFFER;")))
(define GL_STEREO ((c-lambda () int "___result = GL_STEREO;")))
(define GL_BITMAP ((c-lambda () int "___result = GL_BITMAP;")))
(define GL_COLOR ((c-lambda () int "___result = GL_COLOR;")))
(define GL_DEPTH ((c-lambda () int "___result = GL_DEPTH;")))
(define GL_STENCIL ((c-lambda () int "___result = GL_STENCIL;")))
(define GL_DITHER ((c-lambda () int "___result = GL_DITHER;")))
(define GL_RGB ((c-lambda () int "___result = GL_RGB;")))
(define GL_RGBA ((c-lambda () int "___result = GL_RGBA;")))

; /* Implementation limits* /
(define GL_MAX_LIST_NESTING ((c-lambda () int "___result = GL_MAX_LIST_NESTING;")))
(define GL_MAX_EVAL_ORDER ((c-lambda () int "___result = GL_MAX_EVAL_ORDER;")))
(define GL_MAX_LIGHTS ((c-lambda () int "___result = GL_MAX_LIGHTS;")))
(define GL_MAX_CLIP_PLANES ((c-lambda () int "___result = GL_MAX_CLIP_PLANES;")))
(define GL_MAX_TEXTURE_SIZE ((c-lambda () int "___result = GL_MAX_TEXTURE_SIZE;")))
(define GL_MAX_PIXEL_MAP_TABLE ((c-lambda () int "___result = GL_MAX_PIXEL_MAP_TABLE;")))
(define GL_MAX_ATTRIB_STACK_DEPTH ((c-lambda () int "___result = GL_MAX_ATTRIB_STACK_DEPTH;")))
(define GL_MAX_MODELVIEW_STACK_DEPTH ((c-lambda () int "___result = GL_MAX_MODELVIEW_STACK_DEPTH;")))		
(define GL_MAX_NAME_STACK_DEPTH ((c-lambda () int "___result = GL_MAX_NAME_STACK_DEPTH;")))
(define GL_MAX_PROJECTION_STACK_DEPTH ((c-lambda () int "___result = GL_MAX_PROJECTION_STACK_DEPTH;")))
(define GL_MAX_TEXTURE_STACK_DEPTH ((c-lambda () int "___result = GL_MAX_TEXTURE_STACK_DEPTH;")))
(define GL_MAX_VIEWPORT_DIMS ((c-lambda () int "___result = GL_MAX_VIEWPORT_DIMS;")))
(define GL_MAX_CLIENT_ATTRIB_STACK_DEPTH ((c-lambda () int "___result = GL_MAX_CLIENT_ATTRIB_STACK_DEPTH;")))

; /* Gets* /
(define GL_ATTRIB_STACK_DEPTH ((c-lambda () int "___result = GL_ATTRIB_STACK_DEPTH;")))
(define GL_CLIENT_ATTRIB_STACK_DEPTH ((c-lambda () int "___result = GL_CLIENT_ATTRIB_STACK_DEPTH;")))
(define GL_COLOR_CLEAR_VALUE ((c-lambda () int "___result = GL_COLOR_CLEAR_VALUE;")))
(define GL_COLOR_WRITEMASK ((c-lambda () int "___result = GL_COLOR_WRITEMASK;")))
(define GL_CURRENT_INDEX ((c-lambda () int "___result = GL_CURRENT_INDEX;")))
(define GL_CURRENT_COLOR ((c-lambda () int "___result = GL_CURRENT_COLOR;")))
(define GL_CURRENT_NORMAL ((c-lambda () int "___result = GL_CURRENT_NORMAL;")))
(define GL_CURRENT_RASTER_COLOR ((c-lambda () int "___result = GL_CURRENT_RASTER_COLOR;")))
(define GL_CURRENT_RASTER_DISTANCE ((c-lambda () int "___result = GL_CURRENT_RASTER_DISTANCE;")))
(define GL_CURRENT_RASTER_INDEX ((c-lambda () int "___result = GL_CURRENT_RASTER_INDEX;")))
(define GL_CURRENT_RASTER_POSITION ((c-lambda () int "___result = GL_CURRENT_RASTER_POSITION;")))
(define GL_CURRENT_RASTER_TEXTURE_COORDS ((c-lambda () int "___result = GL_CURRENT_RASTER_TEXTURE_COORDS;")))
(define GL_CURRENT_RASTER_POSITION_VALID ((c-lambda () int "___result = GL_CURRENT_RASTER_POSITION_VALID;")))
(define GL_CURRENT_TEXTURE_COORDS ((c-lambda () int "___result = GL_CURRENT_TEXTURE_COORDS;")))
(define GL_INDEX_CLEAR_VALUE ((c-lambda () int "___result = GL_INDEX_CLEAR_VALUE;")))
(define GL_INDEX_MODE ((c-lambda () int "___result = GL_INDEX_MODE;")))
(define GL_INDEX_WRITEMASK ((c-lambda () int "___result = GL_INDEX_WRITEMASK;")))
(define GL_MODELVIEW_MATRIX ((c-lambda () int "___result = GL_MODELVIEW_MATRIX;")))
(define GL_MODELVIEW_STACK_DEPTH ((c-lambda () int "___result = GL_MODELVIEW_STACK_DEPTH;")))
(define GL_NAME_STACK_DEPTH ((c-lambda () int "___result = GL_NAME_STACK_DEPTH;")))
(define GL_PROJECTION_MATRIX ((c-lambda () int "___result = GL_PROJECTION_MATRIX;")))
(define GL_PROJECTION_STACK_DEPTH ((c-lambda () int "___result = GL_PROJECTION_STACK_DEPTH;")))
(define GL_RENDER_MODE ((c-lambda () int "___result = GL_RENDER_MODE;")))
(define GL_RGBA_MODE ((c-lambda () int "___result = GL_RGBA_MODE;")))
(define GL_TEXTURE_MATRIX ((c-lambda () int "___result = GL_TEXTURE_MATRIX;")))
(define GL_TEXTURE_STACK_DEPTH ((c-lambda () int "___result = GL_TEXTURE_STACK_DEPTH;")))
(define GL_VIEWPORT ((c-lambda () int "___result = GL_VIEWPORT;")))

; /* Evaluators* /
(define GL_AUTO_NORMAL ((c-lambda () int "___result = GL_AUTO_NORMAL;")))
(define GL_MAP1_COLOR_4 ((c-lambda () int "___result = GL_MAP1_COLOR_4;")))
(define GL_MAP1_INDEX ((c-lambda () int "___result = GL_MAP1_INDEX;")))
(define GL_MAP1_NORMAL ((c-lambda () int "___result = GL_MAP1_NORMAL;")))
(define GL_MAP1_TEXTURE_COORD_1 ((c-lambda () int "___result = GL_MAP1_TEXTURE_COORD_1;")))
(define GL_MAP1_TEXTURE_COORD_2 ((c-lambda () int "___result = GL_MAP1_TEXTURE_COORD_2;")))
(define GL_MAP1_TEXTURE_COORD_3 ((c-lambda () int "___result = GL_MAP1_TEXTURE_COORD_3;")))
(define GL_MAP1_TEXTURE_COORD_4 ((c-lambda () int "___result = GL_MAP1_TEXTURE_COORD_4;")))
(define GL_MAP1_VERTEX_3 ((c-lambda () int "___result = GL_MAP1_VERTEX_3;")))
(define GL_MAP1_VERTEX_4 ((c-lambda () int "___result = GL_MAP1_VERTEX_4;")))
(define GL_MAP2_COLOR_4 ((c-lambda () int "___result = GL_MAP2_COLOR_4;")))
(define GL_MAP2_INDEX ((c-lambda () int "___result = GL_MAP2_INDEX;")))
(define GL_MAP2_NORMAL ((c-lambda () int "___result = GL_MAP2_NORMAL;")))
(define GL_MAP2_TEXTURE_COORD_1 ((c-lambda () int "___result = GL_MAP2_TEXTURE_COORD_1;")))
(define GL_MAP2_TEXTURE_COORD_2 ((c-lambda () int "___result = GL_MAP2_TEXTURE_COORD_2;")))
(define GL_MAP2_TEXTURE_COORD_3 ((c-lambda () int "___result = GL_MAP2_TEXTURE_COORD_3;")))
(define GL_MAP2_TEXTURE_COORD_4 ((c-lambda () int "___result = GL_MAP2_TEXTURE_COORD_4;")))
(define GL_MAP2_VERTEX_3 ((c-lambda () int "___result = GL_MAP2_VERTEX_3;")))
(define GL_MAP2_VERTEX_4 ((c-lambda () int "___result = GL_MAP2_VERTEX_4;")))
(define GL_MAP1_GRID_DOMAIN ((c-lambda () int "___result = GL_MAP1_GRID_DOMAIN;")))
(define GL_MAP1_GRID_SEGMENTS ((c-lambda () int "___result = GL_MAP1_GRID_SEGMENTS;")))
(define GL_MAP2_GRID_DOMAIN ((c-lambda () int "___result = GL_MAP2_GRID_DOMAIN;")))
(define GL_MAP2_GRID_SEGMENTS ((c-lambda () int "___result = GL_MAP2_GRID_SEGMENTS;")))
(define GL_COEFF ((c-lambda () int "___result = GL_COEFF;")))
(define GL_ORDER ((c-lambda () int "___result = GL_ORDER;")))
(define GL_DOMAIN ((c-lambda () int "___result = GL_DOMAIN;")))

; /* Hints* /
(define GL_PERSPECTIVE_CORRECTION_HINT ((c-lambda () int "___result = GL_PERSPECTIVE_CORRECTION_HINT;")))
(define GL_POINT_SMOOTH_HINT ((c-lambda () int "___result = GL_POINT_SMOOTH_HINT;")))
(define GL_LINE_SMOOTH_HINT ((c-lambda () int "___result = GL_LINE_SMOOTH_HINT;")))
(define GL_POLYGON_SMOOTH_HINT ((c-lambda () int "___result = GL_POLYGON_SMOOTH_HINT;")))
(define GL_FOG_HINT ((c-lambda () int "___result = GL_FOG_HINT;")))
(define GL_DONT_CARE ((c-lambda () int "___result = GL_DONT_CARE;")))
(define GL_FASTEST ((c-lambda () int "___result = GL_FASTEST;")))
(define GL_NICEST ((c-lambda () int "___result = GL_NICEST;")))

; /* Scissor box* /
(define GL_SCISSOR_BOX ((c-lambda () int "___result = GL_SCISSOR_BOX;")))
(define GL_SCISSOR_TEST ((c-lambda () int "___result = GL_SCISSOR_TEST;")))

; /* Pixel Mode / Transfer* /
(define GL_MAP_COLOR ((c-lambda () int "___result = GL_MAP_COLOR;")))
(define GL_MAP_STENCIL ((c-lambda () int "___result = GL_MAP_STENCIL;")))
(define GL_INDEX_SHIFT ((c-lambda () int "___result = GL_INDEX_SHIFT;")))
(define GL_INDEX_OFFSET ((c-lambda () int "___result = GL_INDEX_OFFSET;")))
(define GL_RED_SCALE ((c-lambda () int "___result = GL_RED_SCALE;")))
(define GL_RED_BIAS ((c-lambda () int "___result = GL_RED_BIAS;")))
(define GL_GREEN_SCALE ((c-lambda () int "___result = GL_GREEN_SCALE;")))
(define GL_GREEN_BIAS ((c-lambda () int "___result = GL_GREEN_BIAS;")))
(define GL_BLUE_SCALE ((c-lambda () int "___result = GL_BLUE_SCALE;")))
(define GL_BLUE_BIAS ((c-lambda () int "___result = GL_BLUE_BIAS;")))
(define GL_ALPHA_SCALE ((c-lambda () int "___result = GL_ALPHA_SCALE;")))
(define GL_ALPHA_BIAS ((c-lambda () int "___result = GL_ALPHA_BIAS;")))
(define GL_DEPTH_SCALE ((c-lambda () int "___result = GL_DEPTH_SCALE;")))
(define GL_DEPTH_BIAS ((c-lambda () int "___result = GL_DEPTH_BIAS;")))
(define GL_PIXEL_MAP_S_TO_S_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_S_TO_S_SIZE;")))
(define GL_PIXEL_MAP_I_TO_I_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_I_SIZE;")))
(define GL_PIXEL_MAP_I_TO_R_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_R_SIZE;")))
(define GL_PIXEL_MAP_I_TO_G_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_G_SIZE;")))
(define GL_PIXEL_MAP_I_TO_B_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_B_SIZE;")))
(define GL_PIXEL_MAP_I_TO_A_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_A_SIZE;")))
(define GL_PIXEL_MAP_R_TO_R_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_R_TO_R_SIZE;")))
(define GL_PIXEL_MAP_G_TO_G_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_G_TO_G_SIZE;")))
(define GL_PIXEL_MAP_B_TO_B_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_B_TO_B_SIZE;")))
(define GL_PIXEL_MAP_A_TO_A_SIZE ((c-lambda () int "___result = GL_PIXEL_MAP_A_TO_A_SIZE;")))
(define GL_PIXEL_MAP_S_TO_S ((c-lambda () int "___result = GL_PIXEL_MAP_S_TO_S;")))
(define GL_PIXEL_MAP_I_TO_I ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_I;")))
(define GL_PIXEL_MAP_I_TO_R ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_R;")))
(define GL_PIXEL_MAP_I_TO_G ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_G;")))
(define GL_PIXEL_MAP_I_TO_B ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_B;")))
(define GL_PIXEL_MAP_I_TO_A ((c-lambda () int "___result = GL_PIXEL_MAP_I_TO_A;")))
(define GL_PIXEL_MAP_R_TO_R ((c-lambda () int "___result = GL_PIXEL_MAP_R_TO_R;")))
(define GL_PIXEL_MAP_G_TO_G ((c-lambda () int "___result = GL_PIXEL_MAP_G_TO_G;")))
(define GL_PIXEL_MAP_B_TO_B ((c-lambda () int "___result = GL_PIXEL_MAP_B_TO_B;")))
(define GL_PIXEL_MAP_A_TO_A ((c-lambda () int "___result = GL_PIXEL_MAP_A_TO_A;")))
(define GL_PACK_ALIGNMENT ((c-lambda () int "___result = GL_PACK_ALIGNMENT;")))
(define GL_PACK_LSB_FIRST ((c-lambda () int "___result = GL_PACK_LSB_FIRST;")))
(define GL_PACK_ROW_LENGTH ((c-lambda () int "___result = GL_PACK_ROW_LENGTH;")))
(define GL_PACK_SKIP_PIXELS ((c-lambda () int "___result = GL_PACK_SKIP_PIXELS;")))
(define GL_PACK_SKIP_ROWS ((c-lambda () int "___result = GL_PACK_SKIP_ROWS;")))
(define GL_PACK_SWAP_BYTES ((c-lambda () int "___result = GL_PACK_SWAP_BYTES;")))
(define GL_UNPACK_ALIGNMENT ((c-lambda () int "___result = GL_UNPACK_ALIGNMENT;")))
(define GL_UNPACK_LSB_FIRST ((c-lambda () int "___result = GL_UNPACK_LSB_FIRST;")))
(define GL_UNPACK_ROW_LENGTH ((c-lambda () int "___result = GL_UNPACK_ROW_LENGTH;")))
(define GL_UNPACK_SKIP_PIXELS ((c-lambda () int "___result = GL_UNPACK_SKIP_PIXELS;")))
(define GL_UNPACK_SKIP_ROWS ((c-lambda () int "___result = GL_UNPACK_SKIP_ROWS;")))
(define GL_UNPACK_SWAP_BYTES ((c-lambda () int "___result = GL_UNPACK_SWAP_BYTES;")))
(define GL_ZOOM_X ((c-lambda () int "___result = GL_ZOOM_X;")))
(define GL_ZOOM_Y ((c-lambda () int "___result = GL_ZOOM_Y;")))

; /* Texture mapping* /
(define GL_TEXTURE_ENV ((c-lambda () int "___result = GL_TEXTURE_ENV;")))
(define GL_TEXTURE_ENV_MODE ((c-lambda () int "___result = GL_TEXTURE_ENV_MODE;")))
(define GL_TEXTURE_1D ((c-lambda () int "___result = GL_TEXTURE_1D;")))
(define GL_TEXTURE_2D ((c-lambda () int "___result = GL_TEXTURE_2D;")))
(define GL_TEXTURE_WRAP_S ((c-lambda () int "___result = GL_TEXTURE_WRAP_S;")))
(define GL_TEXTURE_WRAP_T ((c-lambda () int "___result = GL_TEXTURE_WRAP_T;")))
(define GL_TEXTURE_MAG_FILTER ((c-lambda () int "___result = GL_TEXTURE_MAG_FILTER;")))
(define GL_TEXTURE_MIN_FILTER ((c-lambda () int "___result = GL_TEXTURE_MIN_FILTER;")))
(define GL_TEXTURE_ENV_COLOR ((c-lambda () int "___result = GL_TEXTURE_ENV_COLOR;")))
(define GL_TEXTURE_GEN_S ((c-lambda () int "___result = GL_TEXTURE_GEN_S;")))
(define GL_TEXTURE_GEN_T ((c-lambda () int "___result = GL_TEXTURE_GEN_T;")))
(define GL_TEXTURE_GEN_MODE ((c-lambda () int "___result = GL_TEXTURE_GEN_MODE;")))
(define GL_TEXTURE_BORDER_COLOR ((c-lambda () int "___result = GL_TEXTURE_BORDER_COLOR;")))
(define GL_TEXTURE_WIDTH ((c-lambda () int "___result = GL_TEXTURE_WIDTH;")))
(define GL_TEXTURE_HEIGHT ((c-lambda () int "___result = GL_TEXTURE_HEIGHT;")))
(define GL_TEXTURE_BORDER ((c-lambda () int "___result = GL_TEXTURE_BORDER;")))
(define GL_TEXTURE_COMPONENTS ((c-lambda () int "___result = GL_TEXTURE_COMPONENTS;")))
(define GL_TEXTURE_RED_SIZE ((c-lambda () int "___result = GL_TEXTURE_RED_SIZE;")))
(define GL_TEXTURE_GREEN_SIZE ((c-lambda () int "___result = GL_TEXTURE_GREEN_SIZE;")))
(define GL_TEXTURE_BLUE_SIZE ((c-lambda () int "___result = GL_TEXTURE_BLUE_SIZE;")))
(define GL_TEXTURE_ALPHA_SIZE ((c-lambda () int "___result = GL_TEXTURE_ALPHA_SIZE;")))
(define GL_TEXTURE_LUMINANCE_SIZE ((c-lambda () int "___result = GL_TEXTURE_LUMINANCE_SIZE;")))
(define GL_TEXTURE_INTENSITY_SIZE ((c-lambda () int "___result = GL_TEXTURE_INTENSITY_SIZE;")))
(define GL_NEAREST_MIPMAP_NEAREST ((c-lambda () int "___result = GL_NEAREST_MIPMAP_NEAREST;")))
(define GL_NEAREST_MIPMAP_LINEAR ((c-lambda () int "___result = GL_NEAREST_MIPMAP_LINEAR;")))
(define GL_LINEAR_MIPMAP_NEAREST ((c-lambda () int "___result = GL_LINEAR_MIPMAP_NEAREST;")))
(define GL_LINEAR_MIPMAP_LINEAR ((c-lambda () int "___result = GL_LINEAR_MIPMAP_LINEAR;")))
(define GL_OBJECT_LINEAR ((c-lambda () int "___result = GL_OBJECT_LINEAR;")))
(define GL_OBJECT_PLANE ((c-lambda () int "___result = GL_OBJECT_PLANE;")))
(define GL_EYE_LINEAR ((c-lambda () int "___result = GL_EYE_LINEAR;")))
(define GL_EYE_PLANE ((c-lambda () int "___result = GL_EYE_PLANE;")))
(define GL_SPHERE_MAP ((c-lambda () int "___result = GL_SPHERE_MAP;")))
(define GL_DECAL ((c-lambda () int "___result = GL_DECAL;")))
(define GL_MODULATE ((c-lambda () int "___result = GL_MODULATE;")))
(define GL_NEAREST ((c-lambda () int "___result = GL_NEAREST;")))
(define GL_REPEAT ((c-lambda () int "___result = GL_REPEAT;")))
(define GL_CLAMP ((c-lambda () int "___result = GL_CLAMP;")))
(define GL_S ((c-lambda () int "___result = GL_S;")))
(define GL_T ((c-lambda () int "___result = GL_T;")))
(define GL_R ((c-lambda () int "___result = GL_R;")))
(define GL_Q ((c-lambda () int "___result = GL_Q;")))
(define GL_TEXTURE_GEN_R ((c-lambda () int "___result = GL_TEXTURE_GEN_R;")))
(define GL_TEXTURE_GEN_Q ((c-lambda () int "___result = GL_TEXTURE_GEN_Q;")))

; /* Utility* /
(define GL_VENDOR ((c-lambda () int "___result = GL_VENDOR;")))
(define GL_RENDERER ((c-lambda () int "___result = GL_RENDERER;")))
(define GL_VERSION ((c-lambda () int "___result = GL_VERSION;")))
(define GL_EXTENSIONS ((c-lambda () int "___result = GL_EXTENSIONS;")))

; /* Errors* /
(define GL_NO_ERROR ((c-lambda () int "___result = GL_NO_ERROR;")))
(define GL_INVALID_ENUM ((c-lambda () int "___result = GL_INVALID_ENUM;")))
(define GL_INVALID_VALUE ((c-lambda () int "___result = GL_INVALID_VALUE;")))
(define GL_INVALID_OPERATION ((c-lambda () int "___result = GL_INVALID_OPERATION;")))
(define GL_STACK_OVERFLOW ((c-lambda () int "___result = GL_STACK_OVERFLOW;")))
(define GL_STACK_UNDERFLOW ((c-lambda () int "___result = GL_STACK_UNDERFLOW;")))
(define GL_OUT_OF_MEMORY ((c-lambda () int "___result = GL_OUT_OF_MEMORY;")))

; /* glPush/PopAttrib bits* /
(define GL_CURRENT_BIT ((c-lambda () int "___result = GL_CURRENT_BIT;")))
(define GL_POINT_BIT ((c-lambda () int "___result = GL_POINT_BIT;")))
(define GL_LINE_BIT ((c-lambda () int "___result = GL_LINE_BIT;")))
(define GL_POLYGON_BIT ((c-lambda () int "___result = GL_POLYGON_BIT;")))
(define GL_POLYGON_STIPPLE_BIT ((c-lambda () int "___result = GL_POLYGON_STIPPLE_BIT;")))
(define GL_PIXEL_MODE_BIT ((c-lambda () int "___result = GL_PIXEL_MODE_BIT;")))
(define GL_LIGHTING_BIT ((c-lambda () int "___result = GL_LIGHTING_BIT;")))
(define GL_FOG_BIT ((c-lambda () int "___result = GL_FOG_BIT;")))
(define GL_DEPTH_BUFFER_BIT ((c-lambda () int "___result = GL_DEPTH_BUFFER_BIT;")))
(define GL_ACCUM_BUFFER_BIT ((c-lambda () int "___result = GL_ACCUM_BUFFER_BIT;")))
(define GL_STENCIL_BUFFER_BIT ((c-lambda () int "___result = GL_STENCIL_BUFFER_BIT;")))
(define GL_VIEWPORT_BIT ((c-lambda () int "___result = GL_VIEWPORT_BIT;")))
(define GL_TRANSFORM_BIT ((c-lambda () int "___result = GL_TRANSFORM_BIT;")))
(define GL_ENABLE_BIT ((c-lambda () int "___result = GL_ENABLE_BIT;")))
(define GL_COLOR_BUFFER_BIT ((c-lambda () int "___result = GL_COLOR_BUFFER_BIT;")))
(define GL_HINT_BIT ((c-lambda () int "___result = GL_HINT_BIT;")))
(define GL_EVAL_BIT ((c-lambda () int "___result = GL_EVAL_BIT;")))
(define GL_LIST_BIT ((c-lambda () int "___result = GL_LIST_BIT;")))
(define GL_TEXTURE_BIT ((c-lambda () int "___result = GL_TEXTURE_BIT;")))
(define GL_SCISSOR_BIT ((c-lambda () int "___result = GL_SCISSOR_BIT;")))
(define GL_ALL_ATTRIB_BITS ((c-lambda () int "___result = GL_ALL_ATTRIB_BITS;")))


; /* OpenGL 1.1* /
(define GL_PROXY_TEXTURE_1D ((c-lambda () int "___result = GL_PROXY_TEXTURE_1D;")))
(define GL_PROXY_TEXTURE_2D ((c-lambda () int "___result = GL_PROXY_TEXTURE_2D;")))
(define GL_TEXTURE_PRIORITY ((c-lambda () int "___result = GL_TEXTURE_PRIORITY;")))
(define GL_TEXTURE_RESIDENT ((c-lambda () int "___result = GL_TEXTURE_RESIDENT;")))
(define GL_TEXTURE_BINDING_1D ((c-lambda () int "___result = GL_TEXTURE_BINDING_1D;")))
(define GL_TEXTURE_BINDING_2D ((c-lambda () int "___result = GL_TEXTURE_BINDING_2D;")))
(define GL_TEXTURE_INTERNAL_FORMAT ((c-lambda () int "___result = GL_TEXTURE_INTERNAL_FORMAT;")))
(define GL_ALPHA4 ((c-lambda () int "___result = GL_ALPHA4;")))
(define GL_ALPHA8 ((c-lambda () int "___result = GL_ALPHA8;")))
(define GL_ALPHA12 ((c-lambda () int "___result = GL_ALPHA12;")))
(define GL_ALPHA16 ((c-lambda () int "___result = GL_ALPHA16;")))
(define GL_LUMINANCE4 ((c-lambda () int "___result = GL_LUMINANCE4;")))
(define GL_LUMINANCE8 ((c-lambda () int "___result = GL_LUMINANCE8;")))
(define GL_LUMINANCE12 ((c-lambda () int "___result = GL_LUMINANCE12;")))
(define GL_LUMINANCE16 ((c-lambda () int "___result = GL_LUMINANCE16;")))
(define GL_LUMINANCE4_ALPHA4 ((c-lambda () int "___result = GL_LUMINANCE4_ALPHA4;")))
(define GL_LUMINANCE6_ALPHA2 ((c-lambda () int "___result = GL_LUMINANCE6_ALPHA2;")))
(define GL_LUMINANCE8_ALPHA8 ((c-lambda () int "___result = GL_LUMINANCE8_ALPHA8;")))
(define GL_LUMINANCE12_ALPHA4 ((c-lambda () int "___result = GL_LUMINANCE12_ALPHA4;")))
(define GL_LUMINANCE12_ALPHA12 ((c-lambda () int "___result = GL_LUMINANCE12_ALPHA12;")))
(define GL_LUMINANCE16_ALPHA16 ((c-lambda () int "___result = GL_LUMINANCE16_ALPHA16;")))
(define GL_INTENSITY ((c-lambda () int "___result = GL_INTENSITY;")))
(define GL_INTENSITY4 ((c-lambda () int "___result = GL_INTENSITY4;")))
(define GL_INTENSITY8 ((c-lambda () int "___result = GL_INTENSITY8;")))
(define GL_INTENSITY12 ((c-lambda () int "___result = GL_INTENSITY12;")))
(define GL_INTENSITY16 ((c-lambda () int "___result = GL_INTENSITY16;")))
(define GL_R3_G3_B2 ((c-lambda () int "___result = GL_R3_G3_B2;")))
(define GL_RGB4 ((c-lambda () int "___result = GL_RGB4;")))
(define GL_RGB5 ((c-lambda () int "___result = GL_RGB5;")))
(define GL_RGB8 ((c-lambda () int "___result = GL_RGB8;")))
(define GL_RGB10 ((c-lambda () int "___result = GL_RGB10;")))
(define GL_RGB12 ((c-lambda () int "___result = GL_RGB12;")))
(define GL_RGB16 ((c-lambda () int "___result = GL_RGB16;")))
(define GL_RGBA2 ((c-lambda () int "___result = GL_RGBA2;")))
(define GL_RGBA4 ((c-lambda () int "___result = GL_RGBA4;")))
(define GL_RGB5_A1 ((c-lambda () int "___result = GL_RGB5_A1;")))
(define GL_RGBA8 ((c-lambda () int "___result = GL_RGBA8;")))
(define GL_RGB10_A2 ((c-lambda () int "___result = GL_RGB10_A2;")))
(define GL_RGBA12 ((c-lambda () int "___result = GL_RGBA12;")))
(define GL_RGBA16 ((c-lambda () int "___result = GL_RGBA16;")))
(define GL_CLIENT_PIXEL_STORE_BIT ((c-lambda () int "___result = GL_CLIENT_PIXEL_STORE_BIT;")))
(define GL_CLIENT_VERTEX_ARRAY_BIT ((c-lambda () int "___result = GL_CLIENT_VERTEX_ARRAY_BIT;")))

(define glClearIndex (c-lambda (GLfloat) void "glClearIndex"))

(define glClearColor (c-lambda( GLclampf GLclampf GLclampf GLclampf ) void "glClearColor"))

(define glClear (c-lambda( GLbitfield ) void "glClear"))

(define glIndexMask (c-lambda( GLuint ) void "glIndexMask"))

(define glColorMask (c-lambda( GLboolean GLboolean GLboolean GLboolean ) void "glColorMask"))

(define glAlphaFunc (c-lambda ( GLenum GLclampf ) void "glAlphaFunc"))

(define glBlendFunc (c-lambda ( GLenum GLenum ) void "glBlendFunc"))

(define glLogicOp (c-lambda ( GLenum ) void "glLogicOp"))

(define glCullFace (c-lambda ( GLenum ) void "glCullFace"))

(define glFrontFace (c-lambda ( GLenum ) void "glFrontFace"))

(define glPointSize (c-lambda ( GLfloat ) void "glPointSize"))

(define glLineWidth (c-lambda ( GLfloat ) void "glLineWidth"))

(define glLineStipple (c-lambda ( GLint GLushort ) void "glLineStipple"))

(define glPolygonMode (c-lambda ( GLenum GLenum ) void "glPolygonMode"))

(define glPolygonOffset (c-lambda ( GLfloat GLfloat ) void "glPolygonOffset"))

(define glPolygonStipple (c-lambda ( GLubyte* ) void "glPolygonStipple"))

(define glGetPolygonStipple (c-lambda ( GLubyte* ) void "glGetPolygonStipple"))

(define glEdgeFlag (c-lambda ( GLboolean ) void "glEdgeFlag"))

(define glEdgeFlagv (c-lambda ( GLboolean* ) void "glEdgeFlagv"))

(define glScissor (c-lambda ( GLint GLint GLsizei GLsizei ) void "glScissor"))

(define glClipPlane (c-lambda ( GLenum GLdouble* ) void "glClipPlane"))

(define glGetClipPlane (c-lambda ( GLenum GLdouble* ) void "glGetClipPlane"))

(define glDrawBuffer (c-lambda ( GLenum ) void "glDrawBuffer"))

(define glReadBuffer (c-lambda ( GLenum ) void "glReadBuffer"))

(define glEnable (c-lambda ( GLenum ) void "glEnable"))

(define glDisable (c-lambda ( GLenum ) void "glDisable"))

(define glIsEnabled (c-lambda ( GLenum ) GLboolean "glIsEnabled"))

(define glEnableClientState (c-lambda ( GLenum ) void "glEnableClientState"))

(define glDisableClientState (c-lambda ( GLenum ) void "glDisableClientState"))

(define glGetBooleanv (c-lambda ( GLenum GLboolean* ) void "glGetBooleanv"))

(define glGetDoublev (c-lambda ( GLenum GLdouble* ) void "glGetDoublev"))

(define glGetFloatv (c-lambda ( GLenum GLfloat* ) void "glGetFloatv"))

(define glGetIntegerv (c-lambda ( GLenum GLint* ) void "glGetIntegerv"))


(define glPushAttrib (c-lambda ( GLbitfield ) void "glPushAttrib"))

(define glPopAttrib (c-lambda ( ) void "glPopAttrib"))


(define glPushClientAttrib (c-lambda ( GLbitfield ) void "glPushClientAttrib"))

(define glPopClientAttrib (c-lambda ( ) void "glPopClientAttrib"))

(define glRenderMode (c-lambda ( GLenum ) GLint "glRenderMode"))

(define glGetError (c-lambda ( ) GLenum "glGetError"))

;(define glGetString (c-lambda ( GLenum ) GLubyte* "glGetString"))

(define glFinish (c-lambda ( ) void "glFinish"))

(define glFlush (c-lambda ( ) void "glFlush"))

(define glHint (c-lambda ( GLenum GLenum ) void "glHint"))


;; /*
;;  * Depth Buffer
;;  */

(define glClearDepth (c-lambda ( GLclampd ) void "glClearDepth"))

(define glDepthFunc (c-lambda ( GLenum ) void "glDepthFunc"))

(define glDepthMask (c-lambda ( GLboolean ) void "glDepthMask"))

(define glDepthRange (c-lambda ( GLclampd GLclampd ) void "glDepthRange"))


;; /*
;;  * Accumulation Buffer
;;  */

(define glClearAccum (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glClearAccum"))

(define glAccum (c-lambda ( GLenum GLfloat ) void "glAccum"))


;; /*
;;  * Transformation
;;  */

(define glMatrixMode (c-lambda ( GLenum ) void "glMatrixMode"))

(define glOrtho (c-lambda ( GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble )
          void "glOrtho"))

(define glFrustum (c-lambda ( GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble )
          void "glFrustum"))

(define glViewport (c-lambda ( GLint GLint GLsizei GLsizei ) void "glViewport"))

(define glPushMatrix (c-lambda () void "glPushMatrix"))

(define glPopMatrix (c-lambda () void "glPopMatrix"))

(define glLoadIdentity (c-lambda () void "glLoadIdentity"))

(define glLoadMatrixd (c-lambda ( GLdouble* ) void "glLoadMatrixd"))
(define glLoadMatrixf (c-lambda ( GLfloat* ) void "glLoadMatrixf"))

(define glMultMatrixd (c-lambda ( GLdouble* ) void "glMultMatrixd"))
(define glMultMatrixf (c-lambda ( GLfloat* ) void "glMultMatrixf"))

(define glRotated (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glRotated"))
(define glRotatef (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glRotatef"))

(define glScaled (c-lambda ( GLdouble GLdouble GLdouble ) void "glScaled"))
(define glScalef (c-lambda ( GLfloat GLfloat GLfloat ) void "glScalef"))
(define glTranslated (c-lambda ( GLdouble GLdouble GLdouble ) void "glTranslated"))
(define glTranslatef (c-lambda ( GLfloat GLfloat GLfloat ) void "glTranslatef"))


;; /*
;;  * Display Lists
;;  */

(define glIsList (c-lambda ( GLuint ) GLboolean "glIsList"))

(define glDeleteLists (c-lambda ( GLuint GLsizei ) void "glDeleteLists"))

(define glGenLists (c-lambda ( GLsizei ) GLuint "glGenLists"))

(define glNewList (c-lambda ( GLuint GLenum ) void "glNewList"))

(define glEndList (c-lambda () void "glEndList"))

(define glCallList (c-lambda ( GLuint ) void "glCallList"))

(define glCallLists (c-lambda ( GLsizei GLenum GLvoid* ) void "glCallLists"))

(define glListBase (c-lambda ( GLuint ) void "glListBase"))


;; /*
;;  * Drawing Functions
;;  */

(define glBegin (c-lambda ( GLenum ) void "glBegin"))

(define glEnd (c-lambda () void "glEnd"))


(define glVertex2d (c-lambda ( GLdouble GLdouble ) void "glVertex2d"))
(define glVertex2f (c-lambda ( GLfloat GLfloat ) void "glVertex2f"))
(define glVertex2i (c-lambda ( GLint GLint ) void "glVertex2i"))
(define glVertex2s (c-lambda ( GLshort GLshort ) void "glVertex2s"))

(define glVertex3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glVertex3d"))
(define glVertex3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glVertex3f"))
(define glVertex3i (c-lambda ( GLint GLint GLint ) void "glVertex3i"))
(define glVertex3s (c-lambda ( GLshort GLshort GLshort ) void "glVertex3s"))

(define glVertex4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glVertex4d"))
(define glVertex4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glVertex4f"))
(define glVertex4i (c-lambda ( GLint GLint GLint GLint ) void "glVertex4i"))
(define glVertex4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glVertex4s"))

(define glVertex2dv (c-lambda ( GLdouble* ) void "glVertex2dv"))
(define glVertex2fv (c-lambda ( GLfloat* ) void "glVertex2fv"))
(define glVertex2iv (c-lambda ( GLint* ) void "glVertex2iv"))
(define glVertex2sv (c-lambda ( GLshort* ) void "glVertex2sv"))

(define glVertex3dv (c-lambda ( GLdouble* ) void "glVertex3dv"))
(define glVertex3fv (c-lambda ( GLfloat* ) void "glVertex3fv"))
(define glVertex3iv (c-lambda ( GLint* ) void "glVertex3iv"))
(define glVertex3sv (c-lambda ( GLshort* ) void "glVertex3sv"))

(define glVertex4dv (c-lambda ( GLdouble* ) void "glVertex4dv"))
(define glVertex4fv (c-lambda ( GLfloat* ) void "glVertex4fv"))
(define glVertex4iv (c-lambda ( GLint* ) void "glVertex4iv"))
(define glVertex4sv (c-lambda ( GLshort* ) void "glVertex4sv"))


(define glNormal3b (c-lambda ( GLbyte GLbyte GLbyte ) void "glNormal3b"))
(define glNormal3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glNormal3d"))
(define glNormal3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glNormal3f"))
(define glNormal3i (c-lambda ( GLint GLint GLint ) void "glNormal3i"))
(define glNormal3s (c-lambda ( GLshort GLshort GLshort ) void "glNormal3s"))

(define glNormal3bv (c-lambda ( GLbyte* ) void "glNormal3bv"))
(define glNormal3dv (c-lambda ( GLdouble* ) void "glNormal3dv"))
(define glNormal3fv (c-lambda ( GLfloat* ) void "glNormal3fv"))
(define glNormal3iv (c-lambda ( GLint* ) void "glNormal3iv"))
(define glNormal3sv (c-lambda ( GLshort* ) void "glNormal3sv"))


(define glIndexd (c-lambda ( GLdouble ) void "glIndexd"))
(define glIndexf (c-lambda ( GLfloat ) void "glIndexf"))
(define glIndexi (c-lambda ( GLint ) void "glIndexi"))
(define glIndexs (c-lambda ( GLshort ) void "glIndexs"))
(define glIndexub (c-lambda ( GLubyte ) void "glIndexub"))

(define glIndexdv (c-lambda ( GLdouble* ) void "glIndexdv"))
(define glIndexfv (c-lambda ( GLfloat* ) void "glIndexfv"))
(define glIndexiv (c-lambda ( GLint* ) void "glIndexiv"))
(define glIndexsv (c-lambda ( GLshort* ) void "glIndexsv"))
(define glIndexubv (c-lambda ( GLubyte* ) void "glIndexubv"))

(define glColor3b (c-lambda ( GLbyte GLbyte GLbyte ) void "glColor3b"))
(define glColor3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glColor3d"))
(define glColor3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glColor3f"))
(define glColor3i (c-lambda ( GLint GLint GLint ) void "glColor3i"))
(define glColor3s (c-lambda ( GLshort GLshort GLshort ) void "glColor3s"))
(define glColor3ub (c-lambda ( GLubyte GLubyte GLubyte ) void "glColor3ub"))
(define glColor3ui (c-lambda ( GLuint GLuint GLuint ) void "glColor3ui"))
(define glColor3us (c-lambda ( GLushort GLushort GLushort ) void "glColor3us"))

(define glColor4b (c-lambda ( GLbyte GLbyte GLbyte GLbyte ) void "glColor4b"))
(define glColor4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glColor4d"))
(define glColor4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glColor4f"))
(define glColor4i (c-lambda ( GLint GLint GLint GLint ) void "glColor4i"))
(define glColor4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glColor4s"))
(define glColor4ub (c-lambda ( GLubyte GLubyte GLubyte GLubyte ) void "glColor4ub"))
(define glColor4ui (c-lambda ( GLuint GLuint GLuint GLuint ) void "glColor4ui"))
(define glColor4us (c-lambda ( GLushort GLushort GLushort GLushort ) void "glColor4us"))


(define glColor3bv (c-lambda ( GLbyte* ) void "glColor3bv"))
(define glColor3dv (c-lambda ( GLdouble* ) void "glColor3dv"))
(define glColor3fv (c-lambda ( GLfloat* ) void "glColor3fv"))
(define glColor3iv (c-lambda ( GLint* ) void "glColor3iv"))
(define glColor3sv (c-lambda ( GLshort* ) void "glColor3sv"))
(define glColor3ubv (c-lambda ( GLubyte* ) void "glColor3ubv"))
(define glColor3uiv (c-lambda ( GLuint* ) void "glColor3uiv"))
(define glColor3usv (c-lambda ( GLushort* ) void "glColor3usv"))

(define glColor4bv (c-lambda ( GLbyte* ) void "glColor4bv"))
(define glColor4dv (c-lambda ( GLdouble* ) void "glColor4dv"))
(define glColor4fv (c-lambda ( GLfloat* ) void "glColor4fv"))
(define glColor4iv (c-lambda ( GLint* ) void "glColor4iv"))
(define glColor4sv (c-lambda ( GLshort* ) void "glColor4sv"))
(define glColor4ubv (c-lambda ( GLubyte* ) void "glColor4ubv"))
(define glColor4uiv (c-lambda ( GLuint* ) void "glColor4uiv"))
(define glColor4usv (c-lambda ( GLushort* ) void "glColor4usv"))


(define glTexCoord1d (c-lambda ( GLdouble ) void "glTexCoord1d"))
(define glTexCoord1f (c-lambda ( GLfloat ) void "glTexCoord1f"))
(define glTexCoord1i (c-lambda ( GLint ) void "glTexCoord1i"))
(define glTexCoord1s (c-lambda ( GLshort ) void "glTexCoord1s"))

(define glTexCoord2d (c-lambda ( GLdouble GLdouble ) void "glTexCoord2d"))
(define glTexCoord2f (c-lambda ( GLfloat GLfloat ) void "glTexCoord2f"))
(define glTexCoord2i (c-lambda ( GLint GLint ) void "glTexCoord2i"))
(define glTexCoord2s (c-lambda ( GLshort GLshort ) void "glTexCoord2s"))

(define glTexCoord3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glTexCoord3d"))
(define glTexCoord3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glTexCoord3f"))
(define glTexCoord3i (c-lambda ( GLint GLint GLint ) void "glTexCoord3i"))
(define glTexCoord3s (c-lambda ( GLshort GLshort GLshort ) void "glTexCoord3s"))

(define glTexCoord4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glTexCoord4d"))
(define glTexCoord4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glTexCoord4f"))
(define glTexCoord4i (c-lambda ( GLint GLint GLint GLint ) void "glTexCoord4i"))
(define glTexCoord4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glTexCoord4s"))

(define glTexCoord1dv (c-lambda ( GLdouble* ) void "glTexCoord1dv"))
(define glTexCoord1fv (c-lambda ( GLfloat* ) void "glTexCoord1fv"))
(define glTexCoord1iv (c-lambda ( GLint* ) void "glTexCoord1iv"))
(define glTexCoord1sv (c-lambda ( GLshort* ) void "glTexCoord1sv"))

(define glTexCoord2dv (c-lambda ( GLdouble* ) void "glTexCoord2dv"))
(define glTexCoord2fv (c-lambda ( GLfloat* ) void "glTexCoord2fv"))
(define glTexCoord2iv (c-lambda ( GLint* ) void "glTexCoord2iv"))
(define glTexCoord2sv (c-lambda ( GLshort* ) void "glTexCoord2sv"))

(define glTexCoord3dv (c-lambda ( GLdouble* ) void "glTexCoord3dv"))
(define glTexCoord3fv (c-lambda ( GLfloat* ) void "glTexCoord3fv"))
(define glTexCoord3iv (c-lambda ( GLint* ) void "glTexCoord3iv"))
(define glTexCoord3sv (c-lambda ( GLshort* ) void "glTexCoord3sv"))

(define glTexCoord4dv (c-lambda ( GLdouble* ) void "glTexCoord4dv"))
(define glTexCoord4fv (c-lambda ( GLfloat* ) void "glTexCoord4fv"))
(define glTexCoord4iv (c-lambda ( GLint* ) void "glTexCoord4iv"))
(define glTexCoord4sv (c-lambda ( GLshort* ) void "glTexCoord4sv"))


(define glRasterPos2d (c-lambda ( GLdouble GLdouble ) void "glRasterPos2d"))
(define glRasterPos2f (c-lambda ( GLfloat GLfloat ) void "glRasterPos2f"))
(define glRasterPos2i (c-lambda ( GLint GLint ) void "glRasterPos2i"))
(define glRasterPos2s (c-lambda ( GLshort GLshort ) void "glRasterPos2s"))

(define glRasterPos3d (c-lambda ( GLdouble GLdouble GLdouble ) void "glRasterPos3d"))
(define glRasterPos3f (c-lambda ( GLfloat GLfloat GLfloat ) void "glRasterPos3f"))
(define glRasterPos3i (c-lambda ( GLint GLint GLint ) void "glRasterPos3i"))
(define glRasterPos3s (c-lambda ( GLshort GLshort GLshort ) void "glRasterPos3s"))

(define glRasterPos4d (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glRasterPos4d"))
(define glRasterPos4f (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glRasterPos4f"))
(define glRasterPos4i (c-lambda ( GLint GLint GLint GLint ) void "glRasterPos4i"))
(define glRasterPos4s (c-lambda ( GLshort GLshort GLshort GLshort ) void "glRasterPos4s"))

(define glRasterPos2dv (c-lambda ( GLdouble* ) void "glRasterPos2dv"))
(define glRasterPos2fv (c-lambda ( GLfloat* ) void "glRasterPos2fv"))
(define glRasterPos2iv (c-lambda ( GLint* ) void "glRasterPos2iv"))
(define glRasterPos2sv (c-lambda ( GLshort* ) void "glRasterPos2sv"))

(define glRasterPos3dv (c-lambda ( GLdouble* ) void "glRasterPos3dv"))
(define glRasterPos3fv (c-lambda ( GLfloat* ) void "glRasterPos3fv"))
(define glRasterPos3iv (c-lambda ( GLint* ) void "glRasterPos3iv"))
(define glRasterPos3sv (c-lambda ( GLshort* ) void "glRasterPos3sv"))

(define glRasterPos4dv (c-lambda ( GLdouble* ) void "glRasterPos4dv"))
(define glRasterPos4fv (c-lambda ( GLfloat* ) void "glRasterPos4fv"))
(define glRasterPos4iv (c-lambda ( GLint* ) void "glRasterPos4iv"))
(define glRasterPos4sv (c-lambda ( GLshort* ) void "glRasterPos4sv"))


(define glRectd (c-lambda ( GLdouble GLdouble GLdouble GLdouble ) void "glRectd"))
(define glRectf (c-lambda ( GLfloat GLfloat GLfloat GLfloat ) void "glRectf"))
(define glRecti (c-lambda ( GLint GLint GLint GLint ) void "glRecti"))
(define glRects (c-lambda ( GLshort GLshort GLshort GLshort ) void "glRects"))


(define glRectdv (c-lambda ( GLdouble* GLdouble* ) void "glRectdv"))
(define glRectfv (c-lambda ( GLfloat* GLfloat* ) void "glRectfv"))
(define glRectiv (c-lambda ( GLint* GLint* ) void "glRectiv"))
(define glRectsv (c-lambda ( GLshort* GLshort* ) void "glRectsv"))


;; /*
;;  * Vertex Arrays  (1.1)
;;  */

(define glVertexPointer (c-lambda ( GLint GLenum GLsizei GLvoid* ) void "glVertexPointer"))

(define glNormalPointer (c-lambda ( GLenum GLsizei GLvoid* ) void "glNormalPointer"))

(define glColorPointer (c-lambda ( GLint GLenum GLsizei GLvoid* ) void "glColorPointer"))

(define glIndexPointer (c-lambda ( GLenum GLsizei GLvoid* ) void "glIndexPointer"))

(define glTexCoordPointer (c-lambda ( GLint GLenum GLsizei GLvoid* ) void "glTexCoordPointer"))

(define glEdgeFlagPointer (c-lambda ( GLsizei GLvoid* ) void "glEdgeFlagPointer"))

(define glGetPointerv (c-lambda ( GLenum GLvoid**) void "glGetPointerv"))

(define glArrayElement (c-lambda ( GLint ) void "glArrayElement"))

(define glDrawArrays (c-lambda ( GLenum GLint GLsizei ) void "glDrawArrays"))

(define glDrawElements (c-lambda ( GLenum GLsizei GLenum GLvoid* ) void "glDrawElements"))

(define glInterleavedArrays (c-lambda ( GLenum GLsizei GLvoid* ) void "glInterleavedArrays"))

;; /*
;;  * Lighting
;;  */

(define glShadeModel (c-lambda ( GLenum ) void "glShadeModel"))

(define glLightf (c-lambda ( GLenum GLenum GLfloat ) void "glLightf"))
(define glLighti (c-lambda ( GLenum GLenum GLint ) void "glLighti"))
(define glLightfv (c-lambda ( GLenum GLenum GLfloat* ) void "glLightfv"))
(define glLightiv (c-lambda ( GLenum GLenum GLint* ) void "glLightiv"))

(define glGetLightfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetLightfv"))
(define glGetLightiv (c-lambda ( GLenum GLenum GLint* ) void "glGetLightiv"))

(define glLightModelf (c-lambda ( GLenum GLfloat ) void "glLightModelf"))
(define glLightModeli (c-lambda ( GLenum GLint ) void "glLightModeli"))
(define glLightModelfv (c-lambda ( GLenum GLfloat* ) void "glLightModelfv"))
(define glLightModeliv (c-lambda ( GLenum GLint* ) void "glLightModeliv"))

(define glMaterialf (c-lambda ( GLenum GLenum GLfloat ) void "glMaterialf"))
(define glMateriali (c-lambda ( GLenum GLenum GLint ) void "glMateriali"))
(define glMaterialfv (c-lambda ( GLenum GLenum GLfloat* ) void "glMaterialfv"))
(define glMaterialiv (c-lambda ( GLenum GLenum GLint* ) void "glMaterialiv"))

(define glGetMaterialfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetMaterialfv"))
(define glGetMaterialiv (c-lambda ( GLenum GLenum GLint* ) void "glGetMaterialiv"))

(define glColorMaterial (c-lambda ( GLenum GLenum ) void "glColorMaterial"))


;; /*
;;  * Raster functions
;;  */

(define glPixelZoom (c-lambda ( GLfloat GLfloat ) void "glPixelZoom"))

(define glPixelStoref (c-lambda ( GLenum GLfloat ) void "glPixelStoref"))
(define glPixelStorei (c-lambda ( GLenum GLint ) void "glPixelStorei"))

(define glPixelTransferf (c-lambda ( GLenum GLfloat ) void "glPixelTransferf"))
(define glPixelTransferi (c-lambda ( GLenum GLint ) void "glPixelTransferi"))

(define glPixelMapfv (c-lambda ( GLenum GLsizei GLfloat* ) void "glPixelMapfv"))
(define glPixelMapuiv (c-lambda ( GLenum GLsizei GLuint* ) void "glPixelMapuiv"))
(define glPixelMapusv (c-lambda ( GLenum GLsizei GLushort* ) void "glPixelMapusv"))

(define glGetPixelMapfv (c-lambda ( GLenum GLfloat* ) void "glGetPixelMapfv"))
(define glGetPixelMapuiv (c-lambda ( GLenum GLuint* ) void "glGetPixelMapuiv"))
(define glGetPixelMapusv (c-lambda ( GLenum GLushort* ) void "glGetPixelMapusv"))

(define glBitmap (c-lambda ( GLsizei GLsizei GLfloat GLfloat GLfloat GLfloat GLubyte* ) void "glBitmap"))

(define glReadPixels (c-lambda ( GLint GLint GLsizei GLsizei GLenum GLenum GLvoid* ) void "glReadPixels"))

(define glDrawPixels (c-lambda ( GLsizei GLsizei GLenum GLenum GLvoid* ) void "glDrawPixels"))

(define glCopyPixels (c-lambda ( GLint GLint GLsizei GLsizei GLenum ) void "glCopyPixels"))

/*
 * Stenciling
 */
(define glStencilFunc (c-lambda ( GLenum GLint GLuint ) void "glStencilFunc"))

(define glStencilMask (c-lambda ( GLuint ) void "glStencilMask"))

(define glStencilOp (c-lambda ( GLenum GLenum GLenum ) void "glStencilOp"))

(define glClearStencil (c-lambda ( GLint ) void "glClearStencil"))



;; /*
;;  * Texture mapping
;;  */

(define glTexGend (c-lambda ( GLenum GLenum GLdouble ) void "glTexGend"))
(define glTexGenf (c-lambda ( GLenum GLenum GLfloat ) void "glTexGenf"))
(define glTexGeni (c-lambda ( GLenum GLenum GLint ) void "glTexGeni"))

(define glTexGendv (c-lambda ( GLenum GLenum GLdouble* ) void "glTexGendv"))
(define glTexGenfv (c-lambda ( GLenum GLenum GLfloat* ) void "glTexGenfv"))
(define glTexGeniv (c-lambda ( GLenum GLenum GLint* ) void "glTexGeniv"))

(define glGetTexGendv (c-lambda ( GLenum GLenum GLdouble* ) void "glGetTexGendv"))
(define glGetTexGenfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetTexGenfv"))
(define glGetTexGeniv (c-lambda ( GLenum GLenum GLint* ) void "glGetTexGeniv"))


(define glTexEnvf (c-lambda ( GLenum GLenum GLfloat ) void "glTexEnvf"))
(define glTexEnvi (c-lambda ( GLenum GLenum GLint ) void "glTexEnvi"))

(define glTexEnvfv (c-lambda ( GLenum GLenum GLfloat* ) void "glTexEnvfv"))
(define glTexEnviv (c-lambda ( GLenum GLenum GLint* ) void "glTexEnviv"))

(define glGetTexEnvfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetTexEnvfv"))
(define glGetTexEnviv (c-lambda ( GLenum GLenum GLint* ) void "glGetTexEnviv"))


(define glTexParameterf (c-lambda ( GLenum GLenum GLfloat ) void "glTexParameterf"))
(define glTexParameteri (c-lambda ( GLenum GLenum GLint ) void "glTexParameteri"))

(define glTexParameterfv (c-lambda ( GLenum GLenum GLfloat* ) void "glTexParameterfv"))
(define glTexParameteriv (c-lambda ( GLenum GLenum GLint* ) void "glTexParameteriv"))

(define glGetTexParameterfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetTexParameterfv"))
(define glGetTexParameteriv (c-lambda ( GLenum GLenum GLint* ) void "glGetTexParameteriv"))

(define glGetTexLevelParameterfv (c-lambda ( GLenum GLint GLenum GLfloat* ) void "glGetTexLevelParameterfv"))
(define glGetTexLevelParameteriv (c-lambda ( GLenum GLint GLenum GLint* ) void "glGetTexLevelParameteriv"))


(define glTexImage1D (c-lambda ( GLenum GLint GLint GLsizei GLint GLenum GLenum GLvoid* ) void "glTexImage1D"))

(define glTexImage2D (c-lambda ( GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum GLvoid* ) void "glTexImage2D"))

(define glGetTexImage (c-lambda ( GLenum GLint GLenum GLenum GLvoid* ) void "glGetTexImage"))


/* 1.1 functions* /

(define glGenTextures (c-lambda ( GLsizei GLuint* ) void "glGenTextures"))

(define glDeleteTextures (c-lambda ( GLsizei GLuint* ) void "glDeleteTextures"))

(define glBindTexture (c-lambda ( GLenum GLuint ) void "glBindTexture"))

(define glPrioritizeTextures (c-lambda ( GLsizei GLuint* GLclampf* ) void "glPrioritizeTextures"))

(define glAreTexturesResident (c-lambda ( GLsizei GLuint* GLboolean* ) GLboolean "glAreTexturesResident"))

(define glIsTexture (c-lambda ( GLuint ) GLboolean "glIsTexture"))


(define glTexSubImage1D (c-lambda ( GLenum GLint GLint GLsizei GLenum GLenum GLvoid* ) void "glTexSubImage1D"))


(define glTexSubImage2D (c-lambda ( GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum GLvoid* ) void "glTexSubImage2D"))


(define glCopyTexImage1D (c-lambda ( GLenum GLint GLenum GLint GLint GLsizei GLint ) void "glCopyTexImage1D"))


(define glCopyTexImage2D (c-lambda ( GLenum GLint GLenum GLint GLint GLsizei GLsizei GLint ) void "glCopyTexImage2D"))


(define glCopyTexSubImage1D (c-lambda ( GLenum GLint GLint GLint GLint GLsizei ) void "glCopyTexSubImage1D"))


(define glCopyTexSubImage2D (c-lambda ( GLenum GLint GLint GLint GLint GLint GLsizei GLsizei ) void "glCopyTexSubImage2D"))


;; /*
;;  * Evaluators
;;  */

(define glMap1d (c-lambda ( GLenum GLdouble GLdouble GLint GLint GLdouble* ) void "glMap1d"))
(define glMap1f (c-lambda ( GLenum GLfloat GLfloat GLint GLint GLfloat* ) void "glMap1f"))

(define glMap2d (c-lambda ( GLenum GLdouble GLdouble GLint GLint GLdouble GLdouble GLint GLint GLdouble* ) void "glMap2d"))
(define glMap2f (c-lambda ( GLenum GLfloat GLfloat GLint GLint GLfloat GLfloat GLint GLint GLfloat* ) void "glMap2f"))

(define glGetMapdv (c-lambda ( GLenum GLenum GLdouble* ) void "glGetMapdv"))
(define glGetMapfv (c-lambda ( GLenum GLenum GLfloat* ) void "glGetMapfv"))
(define glGetMapiv (c-lambda ( GLenum GLenum GLint* ) void "glGetMapiv"))

(define glEvalCoord1d (c-lambda ( GLdouble ) void "glEvalCoord1d"))
(define glEvalCoord1f (c-lambda ( GLfloat ) void "glEvalCoord1f"))

(define glEvalCoord1dv (c-lambda ( GLdouble* ) void "glEvalCoord1dv"))
(define glEvalCoord1fv (c-lambda ( GLfloat* ) void "glEvalCoord1fv"))

(define glEvalCoord2d (c-lambda ( GLdouble GLdouble ) void "glEvalCoord2d"))
(define glEvalCoord2f (c-lambda ( GLfloat GLfloat ) void "glEvalCoord2f"))

(define glEvalCoord2dv (c-lambda ( GLdouble* ) void "glEvalCoord2dv"))
(define glEvalCoord2fv (c-lambda ( GLfloat* ) void "glEvalCoord2fv"))

(define glMapGrid1d (c-lambda ( GLint GLdouble GLdouble ) void "glMapGrid1d"))
(define glMapGrid1f (c-lambda ( GLint GLfloat GLfloat ) void "glMapGrid1f"))

(define glMapGrid2d (c-lambda ( GLint GLdouble GLdouble GLint GLdouble GLdouble ) void "glMapGrid2d"))
(define glMapGrid2f (c-lambda ( GLint GLfloat GLfloat GLint GLfloat GLfloat ) void "glMapGrid2f"))

(define glEvalPoint1 (c-lambda ( GLint ) void "glEvalPoint1"))

(define glEvalPoint2 (c-lambda ( GLint GLint ) void "glEvalPoint2"))

(define glEvalMesh1 (c-lambda ( GLenum GLint GLint ) void "glEvalMesh1"))

(define glEvalMesh2 (c-lambda ( GLenum GLint GLint GLint GLint ) void "glEvalMesh2"))


;; /*
;;  * Fog
;;  */

(define glFogf (c-lambda ( GLenum GLfloat ) void "glFogf"))

(define glFogi (c-lambda ( GLenum GLint ) void "glFogi"))

(define glFogfv (c-lambda ( GLenum GLfloat* ) void "glFogfv"))

(define glFogiv (c-lambda ( GLenum GLint* ) void "glFogiv"))


;; /*
;;  * Selection and Feedback
;;  */

(define glFeedbackBuffer (c-lambda ( GLsizei GLenum GLfloat* ) void "glFeedbackBuffer"))

(define glPassThrough (c-lambda ( GLfloat ) void "glPassThrough"))

(define glSelectBuffer (c-lambda ( GLsizei GLuint* ) void "glSelectBuffer"))

(define glInitNames (c-lambda () void "glInitNames"))

(define glLoadName (c-lambda ( GLuint ) void "glLoadName"))

(define glPushName (c-lambda ( GLuint ) void "glPushName"))

(define glPopName (c-lambda () void "glPopName"))
