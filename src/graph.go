package cicero

import (
	"errors"
	"github.com/go-echarts/go-echarts/v2/charts"
	"github.com/go-echarts/go-echarts/v2/components"
	"github.com/go-echarts/go-echarts/v2/opts"
	"io"
	"math"
)

type WorkflowGraphType uint

const (
	WorkflowGraphTypeFlow WorkflowGraphType = iota
	WorkflowGraphTypeInputs
)

func WorkflowGraphTypeStrings() []string {
	return []string{"flow", "inputs"}
}

func (t WorkflowGraphType) String() string {
	return WorkflowGraphTypeStrings()[t]
}

func WorkflowGraphTypeFromString(s string) (WorkflowGraphType, error) {
	for i := 0; i < 2; i += 1 {
		if WorkflowGraphType(i).String() == s {
			return WorkflowGraphType(i), nil
		}
	}
	return 0, errors.New("no such WorkflowGraphType: " + s)
}

func RenderWorkflowGraph(wf *WorkflowDefinition, graphType WorkflowGraphType, w io.Writer) error {
	const SymbolSize = 50
	const FontSize = 14

	// XXX nodes := make([]opts.GraphNode, len(*steps))
	nodes := make([]opts.GraphNode, 0)
	for name, step := range wf.Steps {
		graphNode := opts.GraphNode{
			Name:       name,
			Symbol:     "circle",
			SymbolSize: SymbolSize,
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
	switch graphType {
	case WorkflowGraphTypeFlow:
		for name, step := range wf.Steps {
			for _, input := range step.Inputs {
				for name2, step2 := range wf.Steps {
					if name == name2 {
						continue
					}
					for success := range step2.Success {
						if input != success {
							continue
						}
						links = append(links, opts.GraphLink{
							Source: name2,
							Target: name,
							Label: &opts.EdgeLabel{
								Show:      true,
								Formatter: success, // FIXME escape placeholders
							},
						})
					}
				}
			}
		}
	case WorkflowGraphTypeInputs:
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
							Label: &opts.EdgeLabel{
								Show:      true,
								Formatter: input, // FIXME escape placeholders
							},
						})
					}
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
			bc.TextStyle = &opts.TextStyle{FontSize: int(math.Round(FontSize * 1.3))}
		},
	)
	graph.AddJSFuncs(GraphResponsiveJs)
	var curveness float32
	switch graphType {
	case WorkflowGraphTypeInputs:
		curveness = .2
	default:
		curveness = 0
	}
	graph.AddSeries("steps", nodes, links,
		charts.WithLabelOpts(
			opts.Label{
				Show:     true,
				FontSize: FontSize,
			},
		),
		charts.WithGraphChartOpts(
			opts.GraphChart{
				Force:              &opts.GraphForce{Repulsion: 10000},
				Roam:               true,
				FocusNodeAdjacency: true,
				Categories: []*opts.GraphCategory{
					{Name: "Not Runnable"},
					{Name: "Runnable"},
				},
				EdgeSymbol: []string{"none", "arrow"},
				EdgeLabel:  &opts.EdgeLabel{FontSize: FontSize},
			},
		),
		charts.WithLineStyleOpts(
			opts.LineStyle{Curveness: curveness},
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
