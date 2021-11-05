package cicero

import (
	"errors"
	"github.com/input-output-hk/cicero/src/model"
	"io"
	"math"
	"strings"

	"github.com/go-echarts/go-echarts/v2/charts"
	"github.com/go-echarts/go-echarts/v2/components"
	"github.com/go-echarts/go-echarts/v2/opts"
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

func RenderWorkflowGraph(wf *model.WorkflowDefinition, graphType WorkflowGraphType, w io.Writer) error {
	const SymbolSize = 50
	const FontSize = 14

	nodes := make([]opts.GraphNode, 0)
	for name, action := range wf.Actions {
		graphNode := opts.GraphNode{
			Name:       name,
			Symbol:     "circle",
			SymbolSize: SymbolSize,
		}
		if action.IsRunnable() {
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
		for name, action := range wf.Actions {
			for _, input := range action.Inputs {
				for name2, action2 := range wf.Actions {
					if name == name2 {
						continue
					}
					for success := range action2.Success {
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
		for name, action := range wf.Actions {
			for _, input := range action.Inputs {
				for name2, action2 := range wf.Actions {
					if name == name2 {
						continue
					}
				Inner:
					for _, input2 := range action2.Inputs {
						if input != input2 {
							continue
						}
						for _, link := range links {
							if link.Source == name2 && link.Target == name {
								for _, label := range strings.Split(link.Label.Formatter, ", ") {
									if label == input {
										continue Inner
									}
								}
								link.Label.Formatter += ", " + input
								continue Inner
							}
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

	var edgeSymbol []string
	switch graphType {
	case WorkflowGraphTypeInputs:
		edgeSymbol = []string{"none"}
	default:
		edgeSymbol = []string{"none", "arrow"}
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
	graph.AddSeries("actions", nodes, links,
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
				EdgeSymbol: edgeSymbol,
				EdgeLabel:  &opts.EdgeLabel{FontSize: FontSize},
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
