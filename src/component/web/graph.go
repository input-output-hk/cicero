package web

/* FIXME everything
import (
	"errors"
	"io"
	"math"
	"strings"

	"github.com/input-output-hk/cicero/src/domain"

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
	for i, s2 := range WorkflowGraphTypeStrings() {
		if s2 == s {
			return WorkflowGraphType(i), nil
		}
	}
	return 0, errors.New("no such WorkflowGraphType: " + s)
}

const symbolSize = 50

func RenderWorkflowGraphFlow(wf domain.WorkflowDefinition, w io.Writer) error {
	nodes := make([]opts.GraphNode, 0)
	links := make([]opts.GraphLink, 0)

	for name, action := range wf.Actions {
		node := opts.GraphNode{
			Name:       name,
			Symbol:     "circle",
			SymbolSize: symbolSize,
		}
		if action.IsDecision() {
			if action.IsRunnable() {
				node.Symbol = "arrow"
				node.Category = 3
			} else {
				node.Symbol = "triangle"
				node.Category = 2
			}
		} else if action.IsRunnable() {
			node.Symbol = "diamond"
			node.Category = 1
			node.Y = 0
			node.X = 0
			node.SymbolSize = symbolSize * 1.5
		}

		nodes = append(nodes, node)
	}

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

	return renderWorkflowGraph(nodes, links, []charts.SeriesOpts{
		charts.WithGraphChartOpts(opts.GraphChart{
			EdgeSymbol: []string{"none", "arrow"},
			Categories: []*opts.GraphCategory{
				{Name: "Action"},
				{Name: "Action (runnable)"},
				{Name: "Decision"},
				{Name: "Decision (runnable)"},
			},
		}),
	}, w)
}

// ContainsString returns true if a string is present in a iteratee.
func containsString(s []string, v string) bool {
	for _, vv := range s {
		if vv == v {
			return true
		}
	}
	return false
}

func RenderWorkflowGraphInputs(wf domain.WorkflowDefinition, state domain.Facts, w io.Writer) error {
	nodes := make([]opts.GraphNode, 0)
	links := make([]opts.GraphLink, 0)

	for _, action := range wf.Actions {
	Node:
		for _, input := range action.Inputs {
			for _, node := range nodes {
				if node.Name == input {
					continue Node
				}
			}

			node := opts.GraphNode{
				Name:       input,
				Symbol:     "circle",
				SymbolSize: symbolSize,
			}
			for fact := range state {
				if fact != input {
					continue
				}
				node.Symbol = "diamond"
				node.Category = 1
				node.Y = 0
				node.X = 0
				node.SymbolSize = symbolSize * 1.5
				break
			}

			nodes = append(nodes, node)
		}
	}

	for _, node := range nodes {
		for name, action := range wf.Actions {
			if !containsString(action.Inputs, node.Name) {
				continue
			}
		Inner:
			for name2, action2 := range wf.Actions {
				if name == name2 || !containsString(action2.Inputs, node.Name) {
					continue
				}

				for _, link := range links {
					if link.Source == name2 && link.Target == name {
						for _, label := range strings.Split(link.Label.Formatter, ", ") {
							if label == node.Name {
								continue Inner
							}
						}
						link.Label.Formatter += ", " + node.Name
						continue Inner
					}
				}

				links = append(links, opts.GraphLink{
					Source: name,
					Target: name2,
					Label: &opts.EdgeLabel{
						Show:      true,
						Formatter: name + ", " + name2, // FIXME escape placeholders
					},
				})
			}
		}
	}

	return renderWorkflowGraph(nodes, links, []charts.SeriesOpts{
		charts.WithGraphChartOpts(opts.GraphChart{
			EdgeSymbol: []string{"none"},
			Categories: []*opts.GraphCategory{
				{Name: "Missing"},
				{Name: "Present"},
			},
		}),
	}, w)
}

func renderWorkflowGraph(nodes []opts.GraphNode, links []opts.GraphLink, seriesOpts []charts.SeriesOpts, w io.Writer) error {
	const fontSize = 14

	graph := charts.NewGraph()
	graph.SetGlobalOptions(
		charts.WithLegendOpts(
			opts.Legend{Show: true},
		),
		func(bc *charts.BaseConfiguration) {
			bc.TextStyle = &opts.TextStyle{FontSize: int(math.Round(fontSize * 1.3))}
		},
	)
	graph.AddJSFuncs(GraphResponsiveJs)

	graph.AddSeries("workflow", nodes, links, append(seriesOpts,
		charts.WithLabelOpts(
			opts.Label{
				Show:     true,
				FontSize: fontSize,
			},
		),
		// not using charts.WithGraphChartOpts
		// so previous settings are not overwritten
		func(s *charts.SingleSeries) {
			s.Force = &opts.GraphForce{Repulsion: 10000}
			s.Roam = true
			s.FocusNodeAdjacency = true
			s.EdgeLabel = &opts.EdgeLabel{FontSize: fontSize}
		},
	)...)

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
*/
