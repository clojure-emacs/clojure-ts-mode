(defn create-vertex-shader! []
  (native/raw "__value = make_box(glCreateShader(GL_VERTEX_SHADER));"))

(defn set-shader-source! [shader source]
  (native/raw "auto const shader(detail::to_int(~{ shader }));
               auto const &source(detail::to_string(~{ source }));
               __value = make_box(glShaderSource(shader, 1, &source.data, nullptr));"))

(defn compile-shader! [shader]
  (native/raw "__value = make_box(glCompileShader(detail::to_int(~{ shader })));"))
