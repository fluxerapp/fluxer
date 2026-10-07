// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ReportFlowOutcome,
	ReportFlowResponse,
	ReportFlowScreen,
	ReportFlowStep,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

export type ReportFlowPhase = 'screen' | 'summary' | 'notice' | 'thanks';

export interface ReportFlowWalk {
	readonly steps: ReadonlyArray<ReportFlowStep>;
	readonly screenId: string;
	readonly phase: ReportFlowPhase;
	readonly noticeId: string | null;
	readonly reportSent: boolean;
	readonly drafts: ReadonlyMap<string, ReadonlySet<string>>;
	readonly direction: 1 | -1;
}

export type ReportFlowScreenKind = 'choice' | 'checklist' | 'info';

export function getReportFlowScreenKind(screen: ReportFlowScreen): ReportFlowScreenKind {
	if (screen.checklist !== null) return 'checklist';
	if (screen.next_screen_id !== null) return 'info';
	return 'choice';
}

export function getReportFlowScreen(flow: ReportFlowResponse, screenId: string): ReportFlowScreen | null {
	return flow.screens.find((screen) => screen.id === screenId) ?? null;
}

export function startReportFlowWalk(flow: ReportFlowResponse): ReportFlowWalk {
	return {
		steps: [],
		screenId: flow.start_screen_id,
		phase: 'screen',
		noticeId: null,
		reportSent: false,
		drafts: new Map(),
		direction: 1,
	};
}

function applyOutcome(walk: ReportFlowWalk, step: ReportFlowStep, outcome: ReportFlowOutcome): ReportFlowWalk {
	switch (outcome.type) {
		case 'screen':
			if (outcome.screen_id === null) return walk;
			return {...walk, steps: [...walk.steps, step], screenId: outcome.screen_id, phase: 'screen', direction: 1};
		case 'submit':
			return {...walk, steps: [...walk.steps, step], phase: 'summary', direction: 1};
		case 'end':
			if (outcome.notice_id !== null) {
				return {...walk, phase: 'notice', noticeId: outcome.notice_id, direction: 1};
			}
			return {...walk, phase: 'thanks', reportSent: false, direction: 1};
		default:
			return walk;
	}
}

export function chooseReportFlowOption(
	flow: ReportFlowResponse,
	walk: ReportFlowWalk,
	optionId: string,
): ReportFlowWalk {
	if (walk.phase !== 'screen') return walk;
	const screen = getReportFlowScreen(flow, walk.screenId);
	const option = screen?.options.find((candidate) => candidate.id === optionId);
	if (!screen || !option) return walk;
	if (getReportFlowScreenKind(screen) === 'info' && option.outcome.type !== 'link') return walk;
	return applyOutcome(walk, {screen_id: screen.id, option_id: option.id}, option.outcome);
}

export function toggleReportFlowItem(flow: ReportFlowResponse, walk: ReportFlowWalk, itemId: string): ReportFlowWalk {
	if (walk.phase !== 'screen') return walk;
	const screen = getReportFlowScreen(flow, walk.screenId);
	if (!screen?.checklist?.items.some((item) => item.id === itemId)) return walk;
	const current = walk.drafts.get(screen.id) ?? new Set<string>();
	const next = new Set(current);
	if (next.has(itemId)) {
		next.delete(itemId);
	} else {
		next.add(itemId);
	}
	const drafts = new Map(walk.drafts);
	drafts.set(screen.id, next);
	return {...walk, drafts};
}

export function getReportFlowCheckedItems(walk: ReportFlowWalk, screenId: string): ReadonlySet<string> {
	return walk.drafts.get(screenId) ?? new Set<string>();
}

export function canContinueReportFlowChecklist(flow: ReportFlowResponse, walk: ReportFlowWalk): boolean {
	const screen = getReportFlowScreen(flow, walk.screenId);
	if (walk.phase !== 'screen' || !screen?.checklist) return false;
	return getReportFlowCheckedItems(walk, screen.id).size >= screen.checklist.min_checked;
}

export function continueReportFlowChecklist(flow: ReportFlowResponse, walk: ReportFlowWalk): ReportFlowWalk {
	const screen = getReportFlowScreen(flow, walk.screenId);
	if (!screen?.checklist || !canContinueReportFlowChecklist(flow, walk)) return walk;
	const checked = getReportFlowCheckedItems(walk, screen.id);
	const itemIds = screen.checklist.items.filter((item) => checked.has(item.id)).map((item) => item.id);
	return applyOutcome(walk, {screen_id: screen.id, item_ids: itemIds}, screen.checklist.outcome);
}

export function continueReportFlowInfo(flow: ReportFlowResponse, walk: ReportFlowWalk): ReportFlowWalk {
	if (walk.phase !== 'screen') return walk;
	const screen = getReportFlowScreen(flow, walk.screenId);
	if (!screen || screen.next_screen_id === null) return walk;
	return {
		...walk,
		steps: [...walk.steps, {screen_id: screen.id}],
		screenId: screen.next_screen_id,
		direction: 1,
	};
}

export function canGoBackInReportFlow(walk: ReportFlowWalk): boolean {
	return (walk.phase === 'screen' || walk.phase === 'summary') && walk.steps.length > 0;
}

export function backReportFlowWalk(walk: ReportFlowWalk): ReportFlowWalk {
	if (!canGoBackInReportFlow(walk)) return walk;
	const last = walk.steps[walk.steps.length - 1];
	const drafts = new Map(walk.drafts);
	if (last.item_ids) {
		drafts.set(last.screen_id, new Set(last.item_ids));
	}
	return {
		...walk,
		steps: walk.steps.slice(0, -1),
		screenId: last.screen_id,
		phase: 'screen',
		drafts,
		direction: -1,
	};
}

export function markReportFlowSent(walk: ReportFlowWalk): ReportFlowWalk {
	if (walk.phase !== 'summary') return walk;
	return {...walk, phase: 'thanks', reportSent: true, direction: 1};
}

export function getReportFlowAnswerLabels(
	flow: ReportFlowResponse,
	steps: ReadonlyArray<ReportFlowStep>,
): Array<string> {
	const labels: Array<string> = [];
	for (const step of steps) {
		const screen = getReportFlowScreen(flow, step.screen_id);
		if (!screen) continue;
		if (step.option_id !== undefined) {
			const option = screen.options.find((candidate) => candidate.id === step.option_id);
			if (option) labels.push(option.label);
			continue;
		}
		if (step.item_ids && screen.checklist) {
			const selected = new Set(step.item_ids);
			for (const item of screen.checklist.items) {
				if (selected.has(item.id)) labels.push(item.label);
			}
		}
	}
	return labels;
}

export function walkToReportFlowOption(flow: ReportFlowResponse, optionId: string): ReportFlowWalk | null {
	const parents = new Map<string, {screenId: string; optionId: string} | null>([[flow.start_screen_id, null]]);
	const queue = [flow.start_screen_id];
	for (let index = 0; index < queue.length; index++) {
		const screen = getReportFlowScreen(flow, queue[index]);
		if (!screen || getReportFlowScreenKind(screen) !== 'choice') continue;
		if (screen.options.some((option) => option.id === optionId)) {
			const path = [optionId];
			for (let parent = parents.get(screen.id); parent; parent = parents.get(parent.screenId)) {
				path.unshift(parent.optionId);
			}
			const walk = path.reduce((current, id) => chooseReportFlowOption(flow, current, id), startReportFlowWalk(flow));
			return walk.steps.length === path.length && (walk.phase === 'screen' || walk.phase === 'summary') ? walk : null;
		}
		for (const option of screen.options) {
			const next = option.outcome.type === 'screen' ? option.outcome.screen_id : null;
			if (next === null || parents.has(next)) continue;
			parents.set(next, {screenId: screen.id, optionId: option.id});
			queue.push(next);
		}
	}
	return null;
}

export function isReportFlowWalkUrgent(flow: ReportFlowResponse, walk: ReportFlowWalk): boolean {
	return walk.steps.some((step) => getReportFlowScreen(flow, step.screen_id)?.urgent === true);
}

export function getReportFlowStepKey(walk: ReportFlowWalk): string {
	return `${walk.phase}:${walk.steps.length}:${walk.screenId}:${walk.noticeId ?? ''}`;
}
