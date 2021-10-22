package cicero

import (
	"github.com/go-echarts/go-echarts/v2/charts"
	"github.com/go-echarts/go-echarts/v2/components"
	"github.com/go-echarts/go-echarts/v2/opts"
	"io"
)

func RenderWorkflowGraph(wf *workflowDefinition, w io.Writer) error {
	// XXX nodes := make([]opts.GraphNode, len(*steps))
	nodes := make([]opts.GraphNode, 0)
	for name, step := range wf.Steps {
		const SymbolSize = 50
		graphNode := opts.GraphNode{
			Name:       name,
			Symbol:     "circle",
			SymbolSize: SymbolSize,
			Category:   0,
		}
		if step.IsRunnable() {
			graphNode.Symbol = "diamond"
			graphNode.Category = 1
			graphNode.Y = 0
			graphNode.X = 0
			graphNode.SymbolSize = SymbolSize * 1.5
		}

		nodes = append(nodes, graphNode)
	}

	links := make([]opts.GraphLink, 0)
	for name, step := range wf.Steps {
		for _, input := range step.Inputs {
			for name2, step2 := range wf.Steps {
				if name == name2 {
					continue
				}
				for _, input2 := range step2.Inputs {
					if input != input2 {
						continue
					}
					links = append(links, opts.GraphLink{
						Source: name,
						Target: name2,
					})
				}
			}
		}
	}

	graph := charts.NewGraph()
	graph.SetGlobalOptions(
		charts.WithLegendOpts(
			opts.Legend{Show: true},
		),
		func(bc *charts.BaseConfiguration) {
			bc.TextStyle = &opts.TextStyle{FontSize: 18}
		},
	)
	graph.AddJSFuncs(GraphResponsiveJs)
	graph.AddSeries("steps", nodes, links,
		charts.WithLabelOpts(
			opts.Label{Show: true},
		),
		charts.WithGraphChartOpts(
			opts.GraphChart{
				Force:              &opts.GraphForce{Repulsion: 2500},
				Roam:               true,
				FocusNodeAdjacency: true,
				Categories: []*opts.GraphCategory{
					{Name: "Not Runnable"},
					{Name: "Runnable"},
				},
			},
		),
	)

	page := components.NewPage()
	page.AddCharts(graph)

	return page.Render(w)
}

const GraphResponsiveJs = `
document.body.style.margin = 0;
document.querySelector('.container').style.margin = 0;
function resize() {
	const item = document.querySelector('.item');
	item.style.height = window.innerHeight + 'px';
	window.echarts.getInstanceByDom(item).resize({
		height: window.innerHeight,
	});
}
resize();
window.onresize = resize;
`
