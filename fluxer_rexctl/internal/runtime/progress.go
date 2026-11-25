// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"fmt"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	readyHintAfter  = 3 * time.Minute
	unpackHintAfter = 30 * time.Second
	rateWindow      = 5 * time.Second
	tickEvery       = time.Second
)

type byteSample struct {
	at    time.Time
	bytes int64
}

type pullImage struct {
	label  string
	seen   time.Time
	done   bool
	layers map[string]string
}

type progress struct {
	mu         sync.Mutex
	u          *ui.UI
	step       *ui.Step
	verb       string
	now        func() time.Time
	images     map[string]*pullImage
	order      []string
	layers     map[string][2]int64
	states     map[string]string
	samples    []byteSample
	containers map[string]engine.Event
	changed    time.Time
	lastBytes  int64
	lastTitle  string
	hinted     bool
}

func newProgress(s *ui.Step, verb string, u *ui.UI) *progress {
	return &progress{u: u, step: s, verb: verb, now: time.Now, images: map[string]*pullImage{}, layers: map[string][2]int64{}, states: map[string]string{}, containers: map[string]engine.Event{}}
}

func Progress(s *ui.Step, verb string, u *ui.UI) func(engine.Event) {
	if s == nil {
		return nil
	}
	return newProgress(s, verb, u).on
}

func watch(s *ui.Step, verb string, u *ui.UI) (func(engine.Event), func()) {
	if s == nil {
		return nil, func() {}
	}
	pr := newProgress(s, verb, u)
	t := time.NewTicker(tickEvery)
	stop := make(chan struct{})
	go func() {
		for {
			select {
			case <-stop:
				return
			case <-t.C:
				pr.tick()
			}
		}
	}()
	var once sync.Once
	return pr.on, func() {
		once.Do(func() {
			t.Stop()
			close(stop)
		})
	}
}

func (pr *progress) tick() {
	pr.mu.Lock()
	defer pr.mu.Unlock()
	if len(pr.containers) > 0 {
		return
	}
	pr.stalled(pr.now())
}

func (pr *progress) stalled(now time.Time) {
	if pr.hinted || pr.changed.IsZero() || !pr.unpacking() || now.Sub(pr.changed) < unpackHintAfter {
		return
	}
	pr.hinted = true
	pr.step.Info("Still unpacking. Large images take a few minutes on slow disks.")
}

func (pr *progress) on(ev engine.Event) {
	pr.mu.Lock()
	defer pr.mu.Unlock()
	if pr.u != nil && pr.u.Verbose() {
		pr.u.Detail("%s %s", ev.ID, strings.TrimSpace(ev.Text+" "+string(ev.Status)))
	}
	switch {
	case ev.Kind == "container":
		pr.containers[ev.ID] = ev
		pr.step.Progress(0, 0)
		pr.step.Rate(0)
		pr.showContainers()
	case ev.Parent != "":
		pr.states[ev.ID] = ev.Text
		pr.image(ev.Parent, false, ev.ID, ev.Text)
		l, known := pr.layers[ev.ID]
		switch {
		case ev.Text == "Downloading" && ev.Total > 0:
			pr.layers[ev.ID] = [2]int64{ev.Current, ev.Total}
		case known && ev.Text != "Downloading":
			pr.layers[ev.ID] = [2]int64{l[1], l[1]}
		case !known && ev.Text != "Extracting" && ev.Total > 0:
			pr.layers[ev.ID] = [2]int64{ev.Current, ev.Total}
		}
		pr.showPull()
	case ev.Kind == "image" || ev.Parent == "" && ev.Total > 0:
		pr.image(ev.ID, ev.Status == engine.EventDone, "", "")
		if ev.Total > 0 {
			pr.step.Title(pr.pullTitle())
			pr.step.Progress(ev.Current, ev.Total)
			return
		}
		pr.showPull()
	}
}

func (pr *progress) image(id string, done bool, layer, state string) {
	img, ok := pr.images[id]
	if !ok {
		img = &pullImage{label: strings.Join(imageNames([]string{strings.TrimPrefix(id, "Image ")}), ""), seen: pr.now(), layers: map[string]string{}}
		pr.images[id] = img
		pr.order = append(pr.order, id)
	}
	if img.done {
		return
	}
	if done {
		img.done = true
		pr.step.Item("image:"+id, img.label, ui.StateOK, ui.Elapsed(pr.now().Sub(img.seen)))
		return
	}
	if layer != "" {
		img.layers[layer] = state
	}
	pr.step.Item("image:"+id, img.label, ui.StateActive, img.activity())
}

func (img *pullImage) activity() string {
	waiting, downloading := false, false
	for _, state := range img.layers {
		switch state {
		case "Extracting":
			return "unpacking"
		case "Downloading", "Verifying Checksum":
			downloading = true
		case "Pulling fs layer", "Waiting":
			waiting = true
		}
	}
	switch {
	case downloading:
		return "downloading"
	case waiting || len(img.layers) == 0:
		return "waiting"
	}
	return "unpacking"
}

func (pr *progress) showContainers() {
	started := 0
	var waiting []string
	for _, ev := range pr.containers {
		switch ev.Text {
		case "Started", "Running", "Healthy", "Exited", "Restarted":
			started++
		case "Waiting":
			started++
			if !slices.Contains(waiting, ev.Service) {
				waiting = append(waiting, ev.Service)
			}
		}
	}
	slices.Sort(waiting)
	showWaiting(pr.step, pr.verb+" Fluxer", started, len(pr.containers), "services started", waiting)
}

func (pr *progress) pullTitle() string {
	done := 0
	for _, img := range pr.images {
		if img.done {
			done++
		}
	}
	title := fmt.Sprintf("Downloading Fluxer, %d of %d images", done, len(pr.images))
	if pr.unpacking() {
		title += ", unpacking"
	}
	return title
}

func (pr *progress) unpacking() bool {
	extracting := false
	for _, state := range pr.states {
		switch state {
		case "Pulling fs layer", "Waiting", "Downloading", "Verifying Checksum":
			return false
		case "Extracting":
			extracting = true
		}
	}
	return extracting
}

func (pr *progress) showPull() {
	var cur, total int64
	for _, l := range pr.layers {
		cur, total = cur+l[0], total+l[1]
	}
	title := pr.pullTitle()
	pr.step.Title(title)
	pr.step.Progress(cur, total)
	now := pr.now()
	if pr.changed.IsZero() || cur != pr.lastBytes || title != pr.lastTitle {
		pr.changed, pr.lastBytes, pr.lastTitle = now, cur, title
	}
	if pr.unpacking() {
		pr.samples = nil
		pr.step.Rate(0)
		pr.stalled(now)
		return
	}
	pr.samples = append(pr.samples, byteSample{at: now, bytes: cur})
	for len(pr.samples) > 1 && now.Sub(pr.samples[0].at) > rateWindow {
		pr.samples = pr.samples[1:]
	}
	first := pr.samples[0]
	if elapsed := now.Sub(first.at); elapsed >= time.Second && cur > first.bytes {
		pr.step.Rate(int64(float64(cur-first.bytes) / elapsed.Seconds()))
		return
	}
	pr.step.Rate(0)
}

func ShowReadiness(s *ui.Step, prefix string) func(engine.Readiness) {
	start, hinted := time.Now(), false
	return func(r engine.Readiness) {
		if s == nil {
			return
		}
		ready, total, waiting := partGrid(s, r)
		showWaiting(s, prefix, ready, total, "parts ready", waiting)
		if !hinted && len(waiting) > 0 && time.Since(start) >= readyHintAfter {
			hinted = true
			s.Info("%s", notReadyHint(waiting))
		}
	}
}

func partGrid(s *ui.Step, r engine.Readiness) (int, int, []string) {
	states := map[string]ui.State{}
	var order []string
	mark := func(services []string, st ui.State) {
		for _, svc := range services {
			name := PartOf(svc)
			cur, seen := states[name]
			if !seen {
				order = append(order, name)
			}
			if !seen || worse(st, cur) {
				states[name] = st
			}
		}
	}
	mark(r.Ready, ui.StateOK)
	mark(r.NotReady, ui.StateActive)
	mark(r.Failed, ui.StateFail)
	names := PartNames()
	slices.SortStableFunc(order, func(a, b string) int { return rank(names, a) - rank(names, b) })
	ready := 0
	var waiting []string
	for _, name := range order {
		st := states[name]
		detail := ""
		switch st {
		case ui.StateOK:
			ready++
		case ui.StateFail:
			waiting = append(waiting, name)
		default:
			detail = "waiting"
			waiting = append(waiting, name)
		}
		s.Item("part:"+name, name, st, detail)
	}
	return ready, len(order), waiting
}

func worse(a, b ui.State) bool {
	order := []ui.State{ui.StateOK, ui.StateActive, ui.StateFail}
	return slices.Index(order, a) > slices.Index(order, b)
}

func notReadyHint(waiting []string) string {
	minutes := int(readyHintAfter.Minutes())
	if len(waiting) == 1 {
		return fmt.Sprintf("%s isn't ready after %d minutes. Run rexctl logs %s in another terminal to see why.", waiting[0], minutes, waiting[0])
	}
	return fmt.Sprintf("%s aren't ready after %d minutes. Run rexctl logs in another terminal to see why.", waitingFor(waiting), minutes)
}

func showWaiting(s *ui.Step, prefix string, done, total int, what string, waiting []string) {
	title := fmt.Sprintf("%s, %d of %d %s", prefix, done, total, what)
	if len(waiting) == 0 {
		s.Title(title)
		return
	}
	s.Waiting(title, waitingFor(waiting))
}

func waitingFor(names []string) string {
	if len(names) > 3 {
		return fmt.Sprintf("%s and %d more", strings.Join(names[:3], ", "), len(names)-3)
	}
	return ui.JoinAnd(names)
}
