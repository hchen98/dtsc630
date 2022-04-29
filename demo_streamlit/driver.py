import pandas as pd

import streamlit as st
from wordcloud import WordCloud

import plotly.express as px
import matplotlib.pyplot as plt

st.set_page_config(layout="wide")

CATEGORY = [
        {'icon': "far fa-copy", 'label':"Left End"},
        {'id':'Copy','icon':"🐙",'label':"Copy"},
        {'icon': "far fa-chart-bar", 'label':"Chart"},#no tooltip message
        {'icon': "far fa-address-book", 'label':"Book"},
        {'id':' Crazy return value 💀','icon': "💀", 'label':"Calendar"},
        {'icon': "far fa-clone", 'label':"Component"},
        {'icon': "fas fa-tachometer-alt", 'label':"Dashboard",'ttip':"I'm the Dashboard tooltip!"}, #can add a tooltip message
        {'icon': "far fa-copy", 'label':"Right End"},
]

text = 'Fun, fun, awesome, awesome, tubular, astounding, superb, great, amazing, amazing, amazing, amazing'
wordcloud = WordCloud().generate(text)

radar_df = pd.DataFrame(
	dict(
		r=[1, 5, 2, 2, 3],
		theta=['processing cost','mechanical properties','chemical stability', 'thermal stability', 'device integration']
	)
)

pie_df = px.data.tips()

options = st.sidebar.multiselect(
	'What category do you want to select?',
	CATEGORY,
	default=None,
)

if len(options) > 0:
	# st.write('You have selected:', options)

	options2 = st.selectbox(
		'What category do you want to select?',
		CATEGORY,
	)

	if len(options2) > 0:
		# Display the generated image:
		fig, ax = plt.subplots()
		ax.imshow(wordcloud, interpolation='bilinear')
		ax.axis("off")
		st.pyplot(fig)

		col1, col2 = st.columns(2)

		radar_chart = px.line_polar(radar_df, r='r', theta='theta', line_close=True)
		radar_chart.update_traces(fill='toself')

		pie_chart = px.pie(pie_df, values='tip', names='day')

		col1.plotly_chart(radar_chart, use_container_width=True)
		col2.plotly_chart(pie_chart, use_container_width=True)