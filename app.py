from pathlib import Path

import streamlit as st
import streamlit.components.v1 as components


ROOT = Path(__file__).parent
SITE = ROOT / "site"

st.set_page_config(
    page_title="Conceitos e Redes das Leis de Inovação",
    page_icon="🔎",
    layout="wide",
    initial_sidebar_state="collapsed",
)


def build_document() -> str:
    """Monta a página estática em um único documento para o componente."""
    html = (SITE / "index.html").read_text(encoding="utf-8")
    css = (SITE / "styles.css").read_text(encoding="utf-8")
    data = (SITE / "data.js").read_text(encoding="utf-8")
    app = (SITE / "app.js").read_text(encoding="utf-8")

    # O iframe precisa usar as rotas estáticas expostas pelo Streamlit.
    html = html.replace('href="styles.css"', "")
    html = html.replace('<script src="data.js"></script>', "")
    html = html.replace('<script src="app.js"></script>', "")
    html = html.replace('src="rli_logo_hero.svg"', 'src="/app/static/rli_logo_hero.svg"')
    html = html.replace('href="png/', 'href="/app/static/png/')
    html = html.replace('href="leis_inovacao_base.xlsx"', 'href="/app/static/leis_inovacao_base.xlsx" download')
    html = html.replace(
        'href="leis_inovacao_textos_integrais.zip"',
        'href="/app/static/leis_inovacao_textos_integrais.zip" download',
    )
    app = app.replace("`png/${id}.png`", "`/app/static/png/${id}.png`")

    html = html.replace("</head>", f"<style>{css}</style></head>")
    html = html.replace("</body>", f"<script>{data}</script><script>{app}</script></body>")
    return html


# Remove a moldura visual padrão; a identidade fica a cargo da página do RLI.
st.markdown(
    """
    <style>
      header[data-testid="stHeader"], #MainMenu, footer {display:none !important;}
      .stAppViewContainer, .main {background:#fff;}
      .block-container {max-width:none; padding:0 !important;}
      iframe[title="streamlit.components.v1.html"] {display:block; width:100%; border:0;}
    </style>
    """,
    unsafe_allow_html=True,
)

components.html(build_document(), height=4600, scrolling=True)
