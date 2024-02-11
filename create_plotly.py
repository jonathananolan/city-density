import dash
from dash import dcc, html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import pandas as pd

# Read the new CSV file
data = pd.read_csv('output.csv')
data['city_country'] = data['city'] + " (" + data['country'] + ")"
all_city_country = data['city_country'].unique()

# Extract unique values for the radio buttons
pop_weighted_values = [v for v in data['population_weighted'].unique() if pd.notna(v)]
include_water_values = [v for v in data['include_water'].unique() if pd.notna(v)]
cumulative_values = [v for v in data['cumulative'].unique() if v]

# Define the cities to be pre-selected
selected_cities = [
    'New York  (United States)', 'Los Angeles  (United States)', 'Melbourne (Australia)',
    'London (United Kingdom)', 'Tokyo (Japan)', 'Shanghai (China)', 'Beijing  (China)'
]

# Initialize the Dash app
app = dash.Dash(__name__)

# Styling for the components
styles = {
    'container': {
        'margin': '2% 10%',
        'fontFamily': 'Arial, sans-serif'
    },
    'title': {
        'textAlign': 'center',
        'marginBottom': '50px'
    }
}

# Define the app layout
app.layout = html.Div(style=styles['container'], children=[
    html.H1("City Metrics Visualization", style=styles['title']),
    
    html.Label("Search and select cities:"),
    dcc.Dropdown(
        id='city-search-dropdown',
        options=[{'label': city, 'value': city} for city in all_city_country],
        multi=True,
        value=selected_cities,
        style={'marginBottom': '20px'}
    ),
    
    html.Label("Select metric to visualize:"),
    dcc.Dropdown(
        id='metric-dropdown',
        options=[
            {'label': 'Population Density', 'value': 'Density'},
            {'label': 'Population', 'value': 'Population'},
            {'label': 'Area', 'value': 'Area'}
        ],
        value='Density',
        style={'marginBottom': '20px'}
    ),
    dcc.Graph(id='metric-graph'),

    html.Div([  # This is the container for the columns

        # Column for "Population Weighted" radio buttons
        html.Div([
            html.Label('Population Weighted', style={'fontWeight': 'bold', 'fontSize': '10px', 'color': '#666'}),
            dcc.RadioItems(
                id='population-weighted-radio',
                options=[{'label': val, 'value': val} for val in pop_weighted_values],
                value='Population weighted density'
            )
        ], style={'display': 'inline-block', 'width': '15%','fontSize': '10px', 'padding': '1%'}),  # Inline styles for column

        # Column for "Include Water" radio buttons
        html.Div([
            html.Label('Include Water', style={'fontWeight': 'bold', 'fontSize': '10px', 'color': '#666'}),
            dcc.RadioItems(
                id='include-water-radio',
                options=[{'label': val, 'value': val} for val in include_water_values],
                value='Not counting water bodies in area calculation'
            )
        ], style={'display': 'inline-block', 'width': '15%', 'fontSize': '10px','padding': '1%'}),  # Inline styles for column

        # Column for "Cumulative" radio buttons
        html.Div([
            html.Label('Cumulative', style={'fontWeight': 'bold', 'fontSize': '10px', 'color': '#666'}),
            dcc.RadioItems(
                id='cumulative-radio',
                options=[{'label': val, 'value': val} for val in cumulative_values],
                value='Not cumulative'
            )
        ], style={'display': 'inline-block', 'width': '15%',  'fontSize': '10px','padding': '1%'})  # Inline styles for column

    ]),
    dcc.Graph(id='city-map', style={'height': '600px'})
])

@app.callback(
    Output('population-weighted-radio', 'options'),
    [Input('metric-dropdown', 'value')]
)
def update_pop_weighted_options(selected_metric):
    filtered_data = data[data['metric_type'] == selected_metric]
    available_values = filtered_data['population_weighted'].unique()
    
    return [{'label': value, 'value': value, 'disabled': value not in available_values} 
            for value in pop_weighted_values]

@app.callback(
    Output('include-water-radio', 'options'),
    [Input('metric-dropdown', 'value')]
)
def update_include_water_options(selected_metric):
    filtered_data = data[data['metric_type'] == selected_metric]
    available_values = filtered_data['include_water'].unique()
    
    return [{'label': value, 'value': value, 'disabled': value not in available_values} 
            for value in include_water_values]

@app.callback(
    Output('cumulative-radio', 'options'),
    [Input('metric-dropdown', 'value')]
)
def update_cumulative_options(selected_metric):
    filtered_data = data[data['metric_type'] == selected_metric]
    available_values = filtered_data['cumulative'].unique()
    
    return [{'label': value, 'value': value, 'disabled': value not in available_values} 
            for value in cumulative_values]


@app.callback(
    [Output('metric-graph', 'figure'),
    Output('city-map', 'figure')],
    [Input('city-search-dropdown', 'value'),
     Input('metric-dropdown', 'value'),
     Input('population-weighted-radio', 'value'),
     Input('include-water-radio', 'value'),
     Input('cumulative-radio', 'value')]
)
def update_graph(selected_cities, metric, pop_weighted, include_water, cumulative):
    filtered_data = data[data['metric_type'] == metric]
    
    # Check if the selected pop_weighted value is valid for the chosen metric, if not choose a valid one
    if pop_weighted not in filtered_data['population_weighted'].unique():
        valid_values = [v for v in pop_weighted_values if v in filtered_data['population_weighted'].unique()]
        pop_weighted = valid_values[0] if valid_values else None

    # Check if the selected include_water value is valid for the chosen metric, if not choose a valid one
    if include_water not in filtered_data['include_water'].unique():
        valid_values = [v for v in include_water_values if v in filtered_data['include_water'].unique()]
        include_water = valid_values[0] if valid_values else None

    # Check if the selected cumulative value is valid for the chosen metric, if not choose a valid one
    if cumulative not in filtered_data['cumulative'].unique():
        valid_values = [v for v in cumulative_values if v in filtered_data['cumulative'].unique()]
        cumulative = valid_values[0] if valid_values else None

    # Now filter the data based on the valid selections
    if pop_weighted:
        filtered_data = filtered_data[filtered_data['population_weighted'] == pop_weighted]
    if include_water:
        filtered_data = filtered_data[filtered_data['include_water'] == include_water]
    if cumulative:
        filtered_data = filtered_data[filtered_data['cumulative'] == cumulative]

    traces = []
    for city in selected_cities:
        city_data = filtered_data[filtered_data['city_country'] == city]
        trace = go.Scatter(
            x=city_data['Distance from the city centrew in KM'], 
            y=city_data['value'],
            mode='lines',
            name=city
        )
        traces.append(trace)
    
    graph_layout = go.Layout(
        title=f"{metric} by Distance",
        xaxis=dict(title="Distance from City Center (KM)", range=[0,30]),
        yaxis=dict(title=metric),
        plot_bgcolor='white',
        paper_bgcolor='white',
    )

    # Generate the map traces using Scattermapbox
    map_traces = []
    for city in selected_cities:
        city_data = data[data['city_country'] == city]
        
        if not city_data.empty:
            map_trace = go.Scattermapbox(
                lon=[city_data['lon'].iloc[0]],
                lat=[city_data['lat'].iloc[0]],
                text=[city],
                marker=dict(size=10, opacity=0.6),
                mode='markers+text',
                textposition='top center',
                name=city
            )
            map_traces.append(map_trace)
    
    map_layout = go.Layout(
        title="Selected Cities on Map",
        autosize=True,
        hovermode='closest',
        mapbox=dict(
            style="open-street-map",
            center=dict(
                lat=25,  # Initial latitude to center the map
                lon=0   # Initial longitude to center the map
            ),
            zoom=1  # Initial zoom level
        )
    )


    # ... [Return the two figures for the line chart and map] ...


    return {'data': traces, 'layout': graph_layout}, {'data': map_traces, 'layout': map_layout}


# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)
