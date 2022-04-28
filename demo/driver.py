import pandas as pd

import streamlit as st
from wordcloud import WordCloud

import plotly.express as px
import matplotlib.pyplot as plt

st.set_page_config(layout="wide")

CATEGORY = [
	'Program Management', 'Manufacturing & Supply Chain',
	'Technical Solutions', 'Developer Relations',
	'Hardware Engineering', 'Partnerships',
	'Product & Customer Support', 'Software Engineering',
	'Data Center & Network', 'Business Strategy', 'Technical Writing',
	'Technical Infrastructure', 'IT & Data Management',
	'Marketing & Communications', 'Network Engineering',
	'Sales & Account Management', 'Sales Operations', 'Finance',
	'Legal & Government Relations', 'Administrative',
	'User Experience & Design', 'People Operations',
	'Real Estate & Workplace Services'
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