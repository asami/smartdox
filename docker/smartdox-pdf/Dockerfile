ARG KROKI_IMAGE=yuzutech/kroki:latest
FROM ${KROKI_IMAGE}

ENV DEBIAN_FRONTEND=noninteractive

USER root

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    bash \
    ca-certificates \
    curl \
    fonts-ipafont-gothic \
    fonts-ipafont-mincho \
    fonts-noto-cjk \
    fonts-noto-color-emoji \
    git \
    latexmk \
    nodejs \
    npm \
    python3 \
    ruby \
    ruby-dev \
    texlive-fonts-recommended \
    texlive-lang-cjk \
    texlive-lang-japanese \
    texlive-latex-extra \
    texlive-luatex \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN gem install --no-document asciidoctor asciidoctor-pdf

RUN npm install -g asciidoctor-kroki

RUN set -eux; \
  mkdir -p /opt/smartdox/themes; \
  printf '%s\n' \
    'extends: default' \
    'font:' \
    '  catalog:' \
    '    SmartDoxJapanese:' \
    '      normal: /usr/share/fonts/opentype/ipafont-gothic/ipag.ttf' \
    '      bold: /usr/share/fonts/opentype/ipafont-gothic/ipag.ttf' \
    '      italic: /usr/share/fonts/opentype/ipafont-gothic/ipag.ttf' \
    '      bold_italic: /usr/share/fonts/opentype/ipafont-gothic/ipag.ttf' \
    '  fallbacks:' \
    '    - SmartDoxJapanese' \
    'base:' \
    '  font_family: SmartDoxJapanese' \
    'heading:' \
    '  font_family: SmartDoxJapanese' \
    'literal:' \
    '  font_family: SmartDoxJapanese' \
    'code:' \
    '  font_family: SmartDoxJapanese' \
    > /opt/smartdox/themes/asciidoctor-pdf-ja.yml

RUN set -eux; \
  mkdir -p /tmp/smartdox-asciidoctor-warm; \
  printf '%s\n' \
    '= SmartDox Asciidoctor warm-up' \
    '' \
    '日本語PDF' \
    > /tmp/smartdox-asciidoctor-warm/warm.adoc; \
  asciidoctor-pdf \
    -a scripts=cjk \
    -a pdf-theme=/opt/smartdox/themes/asciidoctor-pdf-ja.yml \
    -o /tmp/smartdox-asciidoctor-warm/warm.pdf \
    /tmp/smartdox-asciidoctor-warm/warm.adoc; \
  rm -rf /tmp/smartdox-asciidoctor-warm

RUN set -eux; \
  mkdir -p /tmp/smartdox-tex-warm; \
  printf '%s\n' \
    '\documentclass[uplatex,a4paper,11pt]{jsarticle}' \
    '\usepackage{geometry}' \
    '\usepackage{longtable}' \
    '\usepackage{graphicx}' \
    '\begin{document}' \
    'SmartDox TeX warm-up: 日本語PDF' \
    '\end{document}' \
    > /tmp/smartdox-tex-warm/uplatex-warm.tex; \
  uplatex -interaction=nonstopmode -halt-on-error -output-directory=/tmp/smartdox-tex-warm /tmp/smartdox-tex-warm/uplatex-warm.tex; \
  dvipdfmx -o /tmp/smartdox-tex-warm/uplatex-warm.pdf /tmp/smartdox-tex-warm/uplatex-warm.dvi; \
  luaotfload-tool --update || true; \
  printf '%s\n' \
    '\documentclass[a4paper,11pt]{ltjsarticle}' \
    '\usepackage{luatexja}' \
    '\begin{document}' \
    'SmartDox LuaTeX warm-up: 日本語PDF' \
    '\end{document}' \
    > /tmp/smartdox-tex-warm/lualatex-warm.tex; \
  lualatex -interaction=nonstopmode -halt-on-error -output-directory=/tmp/smartdox-tex-warm /tmp/smartdox-tex-warm/lualatex-warm.tex; \
  rm -rf /tmp/smartdox-tex-warm

COPY smartdox-pdf /usr/local/bin/smartdox-pdf
RUN chmod +x /usr/local/bin/smartdox-pdf

WORKDIR /work

ENTRYPOINT ["/usr/local/bin/smartdox-pdf"]
CMD ["bash"]
