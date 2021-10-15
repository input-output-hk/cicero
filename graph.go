package main

import (
	"io"
	"github.com/go-echarts/go-echarts/v2/charts"
	"github.com/go-echarts/go-echarts/v2/opts"
	"github.com/go-echarts/go-echarts/v2/components"
)

func RenderWorkflowGraph(wf *workflowDefinition, w io.Writer) error {
	// XXX nodes := make([]opts.GraphNode, len(*tasks))
	nodes := make([]opts.GraphNode, 0)
	for name, task := range wf.Tasks {
		graphNode := opts.GraphNode{
			Name:       name,
			Symbol:     "circle",
			SymbolSize: 25,
		}
		if task.Run != nil {
			graphNode.Symbol = "triangle"
			graphNode.Category = 0
			graphNode.Y = 10
			graphNode.SymbolSize = 25 * 1.5
		}

		nodes = append(nodes, graphNode)
	}

	links := make([]opts.GraphLink, 0)
	for name, task := range wf.Tasks {
		for _, input := range task.Inputs {
			for name2, task2 := range wf.Tasks {
				if name == name2 {
					continue
				}
				for _, input2 := range task2.Inputs {
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
		charts.WithToolboxOpts(opts.Toolbox{Show: true}),
	)
	graph.AddJSFuncs(GraphResponsiveJs)
	graph.AddSeries("tasks", nodes, links,
		charts.WithGraphChartOpts(
			opts.GraphChart{
				Force:              &opts.GraphForce{Repulsion: 1000},
				Roam:               true,
				FocusNodeAdjacency: true,
				Categories: []*opts.GraphCategory{
					{
						Name: "",
						Label: &opts.Label{
							Show: true,
						},
					},
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
