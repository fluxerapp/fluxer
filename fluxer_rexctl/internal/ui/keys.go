// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"strings"

	tea "charm.land/bubbletea/v2"
)

type keyWaitModel struct {
	want string
	got  bool
	done bool
}

func (m *keyWaitModel) Init() tea.Cmd { return nil }

func (m *keyWaitModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	if k, ok := msg.(tea.KeyPressMsg); ok {
		m.got = k.String() == m.want
		m.done = true
		return m, tea.Quit
	}
	return m, nil
}

func (m *keyWaitModel) View() tea.View { return tea.NewView("") }

func (u *UI) waitKey(want string) bool {
	if !u.interactive || !u.liveCapable() {
		return false
	}
	final, err := u.runProgram(&keyWaitModel{want: want}, true)
	if err != nil {
		return false
	}
	km, ok := final.(*keyWaitModel)
	return ok && km.got
}

type partyModel struct {
	rex  *rexMotion
	lk   look
	side string
	done bool
}

func newPartyModel(lk look, side string) *partyModel {
	r := newRexMotion(lk, MoodParty)
	return &partyModel{rex: r, lk: lk, side: side}
}

func (m *partyModel) Init() tea.Cmd { return tea.Batch(m.rex.celebrate(), m.rex.hop()) }

func (m *partyModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.lk.width = msg.Width
		m.rex.lk.width = msg.Width
	case tea.KeyPressMsg:
		m.done = true
		m.rex.party = false
		return m, tea.Quit
	}
	cmd := m.rex.update(msg)
	if !m.rex.party && m.rex.resting() {
		m.done = true
		return m, tea.Quit
	}
	return m, cmd
}

func (m *partyModel) View() tea.View {
	if m.done {
		m.rex.party = false
	}
	rows := trimBlank(sayView(m.rex, m.lk, m.side, -1, ToneOK))
	return tea.NewView(strings.Join(rows, "\n") + "\n")
}
