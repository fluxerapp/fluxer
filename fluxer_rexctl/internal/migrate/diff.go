// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"cmp"
	_ "embed"
	"encoding/json"
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"

	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/x/ansi"
	"github.com/compose-spec/compose-go/v2/types"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

//go:embed measurements.json
var measurementsJSON []byte

const MeasurementsSchema = 1

const (
	LoadIdle = "idle"
	LoadBusy = "load"
)

type Measurements struct {
	Schema int        `json:"schema"`
	Basis  Basis      `json:"basis"`
	Runs   []Measured `json:"runs"`
}

type Basis struct {
	VM          string            `json:"vm"`
	CPUs        int               `json:"cpus"`
	MemoryBytes int64             `json:"memory_bytes"`
	Disk        string            `json:"disk"`
	Versions    map[string]string `json:"versions"`
	Date        string            `json:"date"`
	Method      string            `json:"method"`
}

type Measured struct {
	Runtime     Runtime  `json:"runtime"`
	Options     []Option `json:"options"`
	Load        string   `json:"load"`
	MemoryBytes int64    `json:"memory_bytes"`
	CPUPercent  float64  `json:"cpu_percent"`
	DiskBytes   int64    `json:"disk_bytes"`
	Processes   int      `json:"processes"`
	Containers  int      `json:"containers"`
	Threads     int      `json:"threads"`
}

func LoadMeasurements() (Measurements, error) {
	var m Measurements
	if err := json.Unmarshal(measurementsJSON, &m); err != nil {
		return Measurements{}, fmt.Errorf("migrate: measurements: %w", err)
	}
	if m.Schema != MeasurementsSchema {
		return Measurements{}, fmt.Errorf("migrate: measurements schema %d, rexctl reads %d", m.Schema, MeasurementsSchema)
	}
	for _, r := range m.Runs {
		if r.Runtime != Compose && r.Runtime != Native {
			return Measurements{}, fmt.Errorf("migrate: measurements: runtime %q", r.Runtime)
		}
		if r.Load != LoadIdle && r.Load != LoadBusy {
			return Measurements{}, fmt.Errorf("migrate: measurements: load %q", r.Load)
		}
		for _, o := range r.Options {
			if o == OptDocker || !slices.Contains(All, o) {
				return Measurements{}, fmt.Errorf("migrate: measurements: option %q", o)
			}
		}
	}
	return m, nil
}

func (m Measurements) Lookup(runtime Runtime, opts []Option, load string) *Measured {
	want := dataOptions(opts)
	for i, r := range m.Runs {
		if r.Runtime == runtime && r.Load == load && slices.Equal(dataOptions(r.Options), want) {
			return &m.Runs[i]
		}
	}
	return nil
}

func dataOptions(opts []Option) []Option {
	return slices.DeleteFunc(Order(opts), func(o Option) bool { return o == OptDocker })
}

type Inventory struct {
	Images   []string
	Services []string
	Volumes  []string
	Ports    []string
	Binaries []string
}

func appendNew(list []string, v string) []string {
	if slices.Contains(list, v) {
		return list
	}
	return append(list, v)
}

func (a Inventory) minus(b Inventory) Inventory {
	minus := func(x, y []string) []string {
		var out []string
		for _, v := range x {
			if !slices.Contains(y, v) {
				out = appendNew(out, v)
			}
		}
		slices.Sort(out)
		return out
	}
	return Inventory{
		Images:   minus(a.Images, b.Images),
		Services: minus(a.Services, b.Services),
		Volumes:  minus(a.Volumes, b.Volumes),
		Ports:    minus(a.Ports, b.Ports),
		Binaries: minus(a.Binaries, b.Binaries),
	}
}

func ComposeInventory(m *types.Project) Inventory {
	var inv Inventory
	if m == nil {
		return inv
	}
	for _, name := range slices.Sorted(maps.Keys(m.Services)) {
		svc := m.Services[name]
		if svc.GetScale() == 0 {
			continue
		}
		inv.Services = append(inv.Services, name)
		if svc.Image != "" {
			inv.Images = appendNew(inv.Images, svc.Image)
		}
		for _, v := range svc.Volumes {
			if v.Type == types.VolumeTypeVolume && v.Source != "" {
				inv.Volumes = appendNew(inv.Volumes, v.Source)
			}
		}
		for _, p := range svc.Ports {
			if p.Published != "" {
				inv.Ports = appendNew(inv.Ports, p.Published+"/"+strings.ToLower(cmp.Or(p.Protocol, "tcp")))
			}
		}
	}
	slices.Sort(inv.Images)
	slices.Sort(inv.Volumes)
	slices.Sort(inv.Ports)
	return inv
}

type nativeChild struct {
	binary string
	ports  []int
}

var nativeChildren = map[string]nativeChild{
	"postgres":        {"postgres", []int{15432}},
	"valkey":          {"valkey-server", []int{16379}},
	"seaweedfs":       {"weed", []int{19333, 19334}},
	"seaweedfs-filer": {"weed", []int{19335, 18333}},
	"seaweedfs-init":  {"", nil},
	"meilisearch":     {"meilisearch", []int{17700}},
	"snowflakes":      {"fluxer-snowflakes", []int{18090}},
	"users":           {"fluxer-users", []int{18091}},
	"messages":        {"fluxer-messages", []int{18092}},
	"gifs":            {"fluxer-gifs", []int{18093}},
	"unfurl":          {"fluxer-unfurl", []int{18094}},
	"api":             {"node", []int{18080}},
	"gateway":         {"fluxer_gateway", []int{18081}},
	"media-proxy":     {"fluxer-media-proxy", []int{18082}},
	"push":            {"fluxer-push", []int{18085}},
	"admin":           {"fluxer-admin", []int{18083}},
	"app-proxy":       {"fluxer-app-proxy", []int{18084}},
	"livekit":         {"livekit-server", []int{17880}},
	rustService:       {"fluxer-server", []int{18083, 18084, 18085, 18090, 18091, 18092, 18093, 18094}},
}

var retired = map[Option][]string{
	OptKV:      {"valkey"},
	OptStorage: {"seaweedfs", "seaweedfs-filer", "seaweedfs-init"},
	OptRust:    {"snowflakes", "users", "messages", "gifs", "unfurl", "push", "admin", "app-proxy"},
}

var nativeDefault = []string{
	"postgres", "valkey", "seaweedfs", "seaweedfs-filer", "seaweedfs-init", "meilisearch",
	"snowflakes", "users", "messages", "gifs", "unfurl", "api", "gateway", "media-proxy", "push", "admin", "app-proxy",
}

func NativeChildren(done []Option, voice bool) []string {
	out := slices.Clone(nativeDefault)
	for _, o := range done {
		out = slices.DeleteFunc(out, func(c string) bool { return slices.Contains(retired[o], c) })
		if o == OptRust {
			out = append(out, rustService)
		}
	}
	if voice {
		out = append(out, "livekit")
	}
	return out
}

func NativeVoice(chosen map[string]string) bool {
	env, err := settings.Effective(stack.Embedded(), chosen, nil, nil)
	if err != nil {
		return false
	}
	return strings.TrimSpace(env[LiveKitEnabledVar]) == "true" && stack.VoiceBundled(env[stack.VoiceBundledKey])
}

const (
	rustService = "rust"
	fluxerd     = "fluxerd"
	ownProgram  = ""
)

var hostedBy = map[string]string{
	"edge":         fluxerd,
	"nats":         fluxerd,
	"static-proxy": fluxerd,
	"objects-init": fluxerd,
	"worker":       "api",
}

func movesTo(svc string, to Runtime, after Inventory) (string, bool) {
	if slices.Contains(retired[OptRust], svc) && slices.Contains(after.Services, rustService) {
		return rustService, true
	}
	if to != Native {
		return "", false
	}
	if slices.Contains(after.Services, svc) {
		return ownProgram, true
	}
	if h, ok := hostedBy[svc]; ok && (h == fluxerd || slices.Contains(after.Services, h)) {
		return h, true
	}
	return "", false
}

type Move struct {
	Into     string
	Services []string
}

func NativeInventory(children []string) Inventory {
	var inv Inventory
	for _, c := range children {
		inv.Services = append(inv.Services, c)
		info := nativeChildren[c]
		if info.binary != "" {
			inv.Binaries = appendNew(inv.Binaries, info.binary)
		}
		for _, p := range info.ports {
			inv.Ports = appendNew(inv.Ports, strconv.Itoa(p)+"/tcp")
		}
	}
	return inv
}

type Diff struct {
	Options    []Option
	Before     *Measured
	After      *Measured
	BeforeBusy *Measured
	AfterBusy  *Measured
	Removed    Inventory
	Moved      []Move
	Containers bool
}

func Target(runtime Runtime, done []Option, opts ...Option) (Runtime, []Option) {
	after := Order(append(slices.Clone(done), opts...))
	if slices.Contains(opts, OptDocker) {
		return Native, after
	}
	return runtime, after
}

func BuildDiff(m Measurements, runtime Runtime, done, opts []Option, before, after Inventory) Diff {
	to, next := Target(runtime, done, opts...)
	d := Diff{
		Options:    Order(opts),
		Before:     m.Lookup(runtime, done, LoadIdle),
		After:      m.Lookup(to, next, LoadIdle),
		BeforeBusy: m.Lookup(runtime, done, LoadBusy),
		AfterBusy:  m.Lookup(to, next, LoadBusy),
		Removed:    before.minus(after),
	}
	if runtime == Compose && to == Native {
		d.Removed = Inventory{Services: slices.Clone(before.Services), Images: before.Images, Volumes: before.Volumes}
		d.Containers = len(before.Services) > 0
	}
	moved := map[string][]string{}
	d.Removed.Services = slices.DeleteFunc(d.Removed.Services, func(svc string) bool {
		into, ok := movesTo(svc, to, after)
		if ok {
			moved[into] = append(moved[into], svc)
		}
		return ok
	})
	for _, into := range []string{ownProgram, fluxerd, "api", rustService} {
		if len(moved[into]) > 0 {
			d.Moved = append(d.Moved, Move{Into: into, Services: moved[into]})
		}
	}
	return d
}

func (d Diff) Measured() bool {
	return d.Before != nil && d.After != nil
}

func (d Diff) Title() string {
	if len(d.Options) == 1 {
		return d.Options[0].Title()
	}
	titles := make([]string, len(d.Options))
	for i, o := range d.Options {
		titles[i] = o.Title()
	}
	return "Together: " + strings.Join(titles, ", ")
}

type Tone int

const (
	Neutral Tone = iota
	Saving
	Cost
)

type Cell struct {
	Text string
	Tone Tone
}

func (d Diff) rows() [][]Cell {
	if !d.Measured() {
		return nil
	}
	var rows [][]Cell
	add := func(label string, before, after *Measured, f metric) {
		if before == nil || after == nil {
			return
		}
		b, a := f.get(before), f.get(after)
		rows = append(rows, []Cell{neutral(label), neutral(f.show(b)), neutral(f.show(a)), f.delta(b, a)})
	}
	add("Memory, idle", d.Before, d.After, memory)
	add("Memory, under load", d.BeforeBusy, d.AfterBusy, memory)
	add("CPU, idle", d.Before, d.After, cpu)
	add("CPU, under load", d.BeforeBusy, d.AfterBusy, cpu)
	add("Disk", d.Before, d.After, disk)
	add("Processes", d.Before, d.After, processes)
	add("Containers", d.Before, d.After, containers)
	add("Threads", d.Before, d.After, threads)
	return rows
}

type metric struct {
	get  func(*Measured) float64
	show func(float64) string
	step func(float64) string
}

func (f metric) delta(before, after float64) Cell {
	switch d := after - before; {
	case d < 0:
		return Cell{"-" + f.step(-d), Saving}
	case d > 0:
		return Cell{"+" + f.step(d), Cost}
	}
	return Cell{"same", Neutral}
}

func count(v float64) string { return strconv.Itoa(int(v)) }

func size(v float64) string { return ui.Bytes(int64(v)) }

var (
	memory = metric{func(m *Measured) float64 { return float64(m.MemoryBytes) }, size, size}
	disk   = metric{func(m *Measured) float64 { return float64(m.DiskBytes) }, size, size}
	cpu    = metric{
		func(m *Measured) float64 { return m.CPUPercent },
		func(v float64) string { return fmt.Sprintf("%.1f%%", v) },
		func(v float64) string { return fmt.Sprintf("%.1f pts", v) },
	}
	processes  = metric{func(m *Measured) float64 { return float64(m.Processes) }, count, count}
	containers = metric{func(m *Measured) float64 { return float64(m.Containers) }, count, count}
	threads    = metric{func(m *Measured) float64 { return float64(m.Threads) }, count, count}
)

func (d Diff) removedRows() [][2]string {
	var rows [][2]string
	add := func(label string, items []string) {
		if len(items) > 0 {
			rows = append(rows, [2]string{label, strings.Join(items, ", ")})
		}
	}
	add("Services", d.Removed.Services)
	if d.Containers {
		rows = append(rows, [2]string{"Containers", "all of them"})
	}
	add("Images", d.Removed.Images)
	add("Volumes", d.Removed.Volumes)
	add("Ports", d.Removed.Ports)
	add("Programs", d.Removed.Binaries)
	return rows
}

func (d Diff) movedRows() [][2]string {
	var rows [][2]string
	for _, m := range d.Moved {
		label := "Into " + m.Into
		if m.Into == ownProgram {
			label = "Own programs"
		}
		rows = append(rows, [2]string{label, strings.Join(m.Services, ", ")})
	}
	return rows
}

type Paint struct {
	Title  func(string) string
	Dim    func(string) string
	Saving func(string) string
	Cost   func(string) string
	Rule   string
}

func PlainPaint() Paint {
	same := func(s string) string { return s }
	return Paint{Title: same, Dim: same, Saving: same, Cost: same, Rule: "-"}
}

func ThemePaint(t ui.Theme) Paint {
	r := func(st lipgloss.Style) func(string) string { return func(s string) string { return st.Render(s) } }
	return Paint{Title: r(t.Heading), Dim: r(t.Dim), Saving: r(t.OK), Cost: r(t.Warn), Rule: "─"}
}

func (p Paint) tone(c Cell) string {
	switch c.Tone {
	case Saving:
		return p.Saving(c.Text)
	case Cost:
		return p.Cost(c.Text)
	}
	return p.Dim(c.Text)
}

const indent = "  "

func grid(p Paint, head []string, rows [][]Cell, total []Cell) []string {
	widths := make([]int, len(head))
	measure := func(cells []Cell) {
		for i, c := range cells {
			widths[i] = max(widths[i], ansi.StringWidth(c.Text))
		}
	}
	headCells := make([]Cell, len(head))
	for i, h := range head {
		headCells[i] = Cell{Text: h}
	}
	measure(headCells)
	for _, r := range rows {
		measure(r)
	}
	if total != nil {
		measure(total)
	}
	line := func(cells []Cell, paint func(int, Cell) string) string {
		var b strings.Builder
		b.WriteString(indent)
		for i, c := range cells {
			pad := strings.Repeat(" ", widths[i]-ansi.StringWidth(c.Text))
			text := paint(i, c)
			if i == 0 {
				b.WriteString(text + pad)
				continue
			}
			b.WriteString("   " + pad + text)
		}
		return strings.TrimRight(b.String(), " ")
	}
	body := func(i int, c Cell) string {
		if i == 0 || c.Tone == Neutral && c.Text != "same" {
			return c.Text
		}
		return p.tone(c)
	}
	out := []string{line(headCells, func(_ int, c Cell) string { return p.Dim(c.Text) })}
	for _, r := range rows {
		out = append(out, line(r, body))
	}
	if total != nil {
		width := len(widths)*3 - 3
		for _, w := range widths {
			width += w
		}
		out = append(out, indent+p.Dim(strings.Repeat(p.Rule, width)))
		out = append(out, line(total, func(i int, c Cell) string {
			if i == 0 {
				return p.Title(c.Text)
			}
			return body(i, c)
		}))
	}
	return out
}

func neutral(s string) Cell { return Cell{Text: s} }

func (d Diff) Lines(p Paint) []string {
	out := []string{"", indent + p.Title(d.Title())}
	if len(d.Options) == 1 {
		out = append(out, indent+d.Options[0].Summary())
	}
	out = append(out, "")
	if rows := d.rows(); len(rows) > 0 {
		out = append(out, grid(p, []string{"Typical", "Now", "After", "Change"}, rows, nil)...)
	} else {
		out = append(out, indent+p.Dim("Typical figures: not measured yet."))
	}
	leavesDocker := slices.Contains(d.Options, OptDocker)
	moved := "Moves"
	if leavesDocker {
		moved = "Moves into the native service"
	}
	out = append(out, section(p, moved, d.movedRows())...)
	out = append(out, section(p, "Goes away", d.removedRows())...)
	if leavesDocker {
		out = append(out, "", indent+p.Dim("The data in these volumes moves into the new instance's data folder. The Docker instance stays stopped, with its volumes and images, until you remove it."))
	}
	return out
}

func section(p Paint, title string, rows [][2]string) []string {
	if len(rows) == 0 {
		return nil
	}
	out := []string{"", indent + title}
	width := 0
	for _, r := range rows {
		width = max(width, len(r[0]))
	}
	for _, r := range rows {
		out = append(out, indent+indent+p.Dim(r[0]+strings.Repeat(" ", width-len(r[0])))+"   "+r[1])
	}
	return out
}

func SummaryLines(p Paint, each []Diff, together *Diff) []string {
	head := []string{"Change", "Memory", "CPU", "Disk", "Processes", "Containers"}
	row := func(label string, d Diff) []Cell {
		cells := []Cell{neutral(label)}
		if !d.Measured() {
			cells = append(cells, Cell{Text: "not measured yet"})
			for range head[2:] {
				cells = append(cells, Cell{})
			}
			return cells
		}
		for _, f := range []metric{memory, cpu, disk, processes, containers} {
			cells = append(cells, f.delta(f.get(d.Before), f.get(d.After)))
		}
		return cells
	}
	var rows [][]Cell
	for _, d := range each {
		rows = append(rows, row(d.Title(), d))
	}
	var total []Cell
	if together != nil && len(each) > 1 {
		total = row("All of them", *together)
	}
	out := []string{"", indent + p.Title("What each change saves, typically"), ""}
	return append(out, grid(p, head, rows, total)...)
}

func Show(u *ui.UI, lines []string) {
	for _, l := range lines {
		u.Info("%s", l)
	}
}

func Paints(u *ui.UI) Paint {
	if u.Mode() != ui.ModeRich {
		return PlainPaint()
	}
	return ThemePaint(u.Theme())
}

func BasisLines(p Paint, m Measurements) []string {
	out := []string{"", indent + p.Title("How the typical figures were measured")}
	b := m.Basis
	if len(m.Runs) == 0 || b.Date == "" {
		return append(out, indent+"These options have not been measured yet.")
	}
	out = append(out, indent+fmt.Sprintf("Measured on %s: %s, %d CPUs, %s memory, %s.", b.Date, b.VM, b.CPUs, ui.Bytes(b.MemoryBytes), b.Disk))
	if len(b.Versions) > 0 {
		var v []string
		for _, k := range slices.Sorted(maps.Keys(b.Versions)) {
			v = append(v, k+" "+b.Versions[k])
		}
		out = append(out, indent+"Versions: "+strings.Join(v, ", ")+".")
	}
	if b.Method != "" {
		out = append(out, indent+"Method: "+b.Method)
	}
	return out
}
