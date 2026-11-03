// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import type {ReportFlowDef} from '@app/api/report/flows/ReportFlowDefinitions';
import type {
	ReportFlowCopyKey,
	ReportFlowLinkId,
	ReportFlowNoticeDef,
	ReportFlowOptionDef,
	ReportFlowOutcomeDef,
	ReportFlowScreenDef,
} from '@app/api/report/flows/ReportFlowScreens';
import {getLegacyCategory, isReportReasonKey} from '@app/api/report/flows/ReportReasonCatalog';
import type {ReportFlowSurface, ReportFlowTargetType} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

export interface ReportFlowLibrary {
	flows: Record<ReportFlowTargetType, ReportFlowDef>;
	screens: ReadonlyArray<ReportFlowScreenDef>;
	notices: ReadonlyArray<ReportFlowNoticeDef>;
}

export interface ReportFlowInstance {
	selfHosted: boolean;
	guidelinesLinked: boolean;
}

type ReportFlowScreenKind = 'choice' | 'checklist' | 'info';

export interface ReportFlowVariantScreen {
	def: ReportFlowScreenDef;
	kind: ReportFlowScreenKind;
	subtitle: ReportFlowCopyKey | null;
	options: ReadonlyArray<ReportFlowOptionDef>;
}

export interface ReportFlowVariant {
	target: ReportFlowTargetType;
	surface: ReportFlowSurface;
	startScreenId: string;
	screens: ReadonlyMap<string, ReportFlowVariantScreen>;
	notices: ReadonlyArray<ReportFlowNoticeDef>;
	revisionHash: string;
}

const REPORT_FLOW_TARGETS: ReadonlyArray<ReportFlowTargetType> = ['message', 'user', 'guild'];
const REPORT_FLOW_SURFACES: ReadonlyArray<ReportFlowSurface> = ['in_app', 'dsa'];
const REPORT_FLOW_VARIANTS = REPORT_FLOW_TARGETS.flatMap((target) =>
	REPORT_FLOW_SURFACES.map((surface) => ({target, surface})),
);
const REPORT_FLOW_ID_PATTERN = /^[a-z0-9_]{1,48}$/;
const MAX_WALK_SCREENS = 10;
const REPORT_FLOW_CHECKED_INSTANCES: ReadonlyArray<ReportFlowInstance> = [
	{selfHosted: false, guidelinesLinked: true},
	{selfHosted: true, guidelinesLinked: false},
	{selfHosted: true, guidelinesLinked: true},
];

function getReportFlowScreenKind(screen: ReportFlowScreenDef): ReportFlowScreenKind {
	if (screen.checklist) {
		return 'checklist';
	}
	if (screen.nextScreenId !== undefined) {
		return 'info';
	}
	return 'choice';
}

function isLinkAvailable(link: ReportFlowLinkId, instance: ReportFlowInstance): boolean {
	switch (link) {
		case 'crisis_lines':
			return true;
		case 'guidelines':
			return instance.guidelinesLinked;
		case 'copyright':
			return !instance.selfHosted;
	}
}

function resolveScreenSubtitle(
	def: ReportFlowScreenDef,
	surface: ReportFlowSurface,
	instance: ReportFlowInstance,
): ReportFlowCopyKey | null {
	const variants = def.subtitleVariants;
	if (surface === 'dsa' && variants?.dsa !== undefined) {
		return variants.dsa;
	}
	if (instance.selfHosted && variants?.selfHosted !== undefined) {
		return variants.selfHosted;
	}
	return def.subtitle ?? null;
}

function isOptionInVariant(
	option: ReportFlowOptionDef,
	surface: ReportFlowSurface,
	instance: ReportFlowInstance,
): boolean {
	if (option.surface !== undefined && option.surface !== surface) {
		return false;
	}
	return option.outcome.type !== 'link' || isLinkAvailable(option.outcome.link, instance);
}

function getScreenEdges(screen: ReportFlowVariantScreen): Array<string> {
	const edges: Array<string> = [];
	for (const option of screen.options) {
		if (option.outcome.type === 'screen') {
			edges.push(option.outcome.screenId);
		}
	}
	if (screen.def.checklist?.outcome.type === 'screen') {
		edges.push(screen.def.checklist.outcome.screenId);
	}
	if (screen.def.nextScreenId !== undefined) {
		edges.push(screen.def.nextScreenId);
	}
	return edges;
}

function getScreenOutcomes(screen: ReportFlowVariantScreen): Array<ReportFlowOutcomeDef> {
	const outcomes = screen.options.map((option) => option.outcome);
	if (screen.def.checklist) {
		outcomes.push(screen.def.checklist.outcome);
	}
	return outcomes;
}

function canonicalOutcome(outcome: ReportFlowOutcomeDef): string {
	switch (outcome.type) {
		case 'screen':
			return `screen:${outcome.screenId}`;
		case 'submit':
			return `submit:${outcome.reason}`;
		case 'end':
			return outcome.noticeId === undefined ? 'end' : `end:${outcome.noticeId}`;
		case 'link':
			return `link:${outcome.link}`;
	}
}

function computeRevisionHash(
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	startScreenId: string,
	screens: ReadonlyMap<string, ReportFlowVariantScreen>,
	notices: ReadonlyArray<ReportFlowNoticeDef>,
): string {
	const canonical = {
		target,
		surface,
		start: startScreenId,
		screens: [...screens.values()]
			.sort((a, b) => {
				if (a.def.id < b.def.id) {
					return -1;
				}
				if (a.def.id > b.def.id) {
					return 1;
				}
				return 0;
			})
			.map((screen) => ({
				id: screen.def.id,
				kind: screen.kind,
				title: screen.def.title,
				subtitle: screen.subtitle,
				options_heading: screen.options.length > 0 ? (screen.def.optionsHeading ?? null) : null,
				urgent: screen.def.urgent === true,
				next: screen.def.nextScreenId ?? null,
				options: screen.options.map((option) => ({
					id: option.id,
					label: option.label,
					outcome: canonicalOutcome(option.outcome),
				})),
				checklist: screen.def.checklist
					? {
							items: screen.def.checklist.items.map((item) => ({
								id: item.id,
								label: item.label,
								description: item.description ?? null,
								reason: item.reason ?? null,
							})),
							min_checked: screen.def.checklist.minChecked,
							outcome: canonicalOutcome(screen.def.checklist.outcome),
						}
					: null,
			})),
		notices: notices.map((notice) => ({id: notice.id, title: notice.title, body: notice.body})),
	};
	return createHash('sha256').update(JSON.stringify(canonical)).digest('hex').slice(0, 16);
}

export function buildReportFlowVariant(
	library: ReportFlowLibrary,
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	instance: ReportFlowInstance,
): ReportFlowVariant | null {
	const startScreenId = library.flows[target].start[surface];
	if (startScreenId === undefined) {
		return null;
	}
	const screenDefs = new Map(library.screens.map((screen) => [screen.id, screen]));
	const screens = new Map<string, ReportFlowVariantScreen>();
	const noticeIds = new Set<string>();
	const queue = [startScreenId];
	for (let index = 0; index < queue.length; index++) {
		const screenId = queue[index];
		if (screens.has(screenId)) {
			continue;
		}
		const def = screenDefs.get(screenId);
		if (!def) {
			throw new Error(`Report flow ${target}/${surface} references an unknown screen: ${screenId}`);
		}
		const screen: ReportFlowVariantScreen = {
			def,
			kind: getReportFlowScreenKind(def),
			subtitle: resolveScreenSubtitle(def, surface, instance),
			options: (def.options ?? []).filter((option) => isOptionInVariant(option, surface, instance)),
		};
		screens.set(screenId, screen);
		for (const outcome of getScreenOutcomes(screen)) {
			if (outcome.type === 'end' && outcome.noticeId !== undefined) {
				noticeIds.add(outcome.noticeId);
			}
		}
		queue.push(...getScreenEdges(screen));
	}
	const notices = library.notices.filter((notice) => noticeIds.has(notice.id));
	return {
		target,
		surface,
		startScreenId,
		screens,
		notices,
		revisionHash: computeRevisionHash(target, surface, startScreenId, screens, notices),
	};
}

function collectDefinitionErrors(library: ReportFlowLibrary): Array<string> {
	const errors: Array<string> = [];
	const screenIds = new Set<string>();
	const noticeIds = new Set(library.notices.map((notice) => notice.id));
	const checkId = (id: string, where: string) => {
		if (!REPORT_FLOW_ID_PATTERN.test(id)) {
			errors.push(`${where}: id "${id}" does not match ${REPORT_FLOW_ID_PATTERN}`);
		}
	};
	const checkKey = (key: string | undefined, where: string) => {
		if (key !== undefined && !Object.hasOwn(CONTENT_I18N_MESSAGES, key)) {
			errors.push(`${where}: unknown copy key ${key}`);
		}
	};
	const checkOutcome = (outcome: ReportFlowOutcomeDef, where: string) => {
		if (outcome.type === 'submit' && !isReportReasonKey(outcome.reason)) {
			errors.push(`${where}: unknown reason ${outcome.reason}`);
		}
		if (outcome.type === 'end' && outcome.noticeId !== undefined && !noticeIds.has(outcome.noticeId)) {
			errors.push(`${where}: unknown notice ${outcome.noticeId}`);
		}
	};
	for (const notice of library.notices) {
		checkId(notice.id, `notice ${notice.id}`);
		checkKey(notice.title, `notice ${notice.id}`);
		checkKey(notice.body, `notice ${notice.id}`);
	}
	if (noticeIds.size !== library.notices.length) {
		errors.push('notice ids are not unique');
	}
	for (const screen of library.screens) {
		const where = `screen ${screen.id}`;
		checkId(screen.id, where);
		if (screenIds.has(screen.id)) {
			errors.push(`${where}: duplicate screen id`);
		}
		screenIds.add(screen.id);
		checkKey(screen.title, where);
		checkKey(screen.subtitle, where);
		checkKey(screen.subtitleVariants?.dsa ?? undefined, where);
		checkKey(screen.subtitleVariants?.selfHosted ?? undefined, where);
		checkKey(screen.optionsHeading, where);
		const options = screen.options ?? [];
		const kind = getReportFlowScreenKind(screen);
		if (kind === 'checklist' && (options.length > 0 || screen.nextScreenId !== undefined)) {
			errors.push(`${where}: a checklist screen has no options and no next screen`);
		}
		if (kind === 'info' && options.some((option) => option.outcome.type !== 'link')) {
			errors.push(`${where}: an info screen only has link options`);
		}
		if (kind === 'choice' && !options.some((option) => option.outcome.type !== 'link')) {
			errors.push(`${where}: a choice screen needs at least one option that is not a link`);
		}
		const optionIds = new Set<string>();
		for (const option of options) {
			const optionWhere = `${where} option ${option.id}`;
			checkId(option.id, optionWhere);
			if (optionIds.has(option.id)) {
				errors.push(`${optionWhere}: duplicate option id`);
			}
			optionIds.add(option.id);
			checkKey(option.label, optionWhere);
			checkOutcome(option.outcome, optionWhere);
			const endsWithoutNotice = option.outcome.type === 'end' && option.outcome.noticeId === undefined;
			const linksToCopyright = option.outcome.type === 'link' && option.outcome.link === 'copyright';
			if ((endsWithoutNotice || linksToCopyright) && option.surface !== 'in_app') {
				errors.push(`${optionWhere}: a no-report or copyright link option must be limited to in_app`);
			}
		}
		if (!screen.checklist) {
			continue;
		}
		const {items, minChecked, outcome} = screen.checklist;
		const itemIds = new Set<string>();
		for (const item of items) {
			const itemWhere = `${where} item ${item.id}`;
			checkId(item.id, itemWhere);
			if (itemIds.has(item.id)) {
				errors.push(`${itemWhere}: duplicate item id`);
			}
			itemIds.add(item.id);
			checkKey(item.label, itemWhere);
			checkKey(item.description, itemWhere);
			if (item.reason !== undefined && !isReportReasonKey(item.reason)) {
				errors.push(`${itemWhere}: unknown reason ${item.reason}`);
			}
		}
		if (!Number.isInteger(minChecked) || minChecked < 1 || minChecked > items.length) {
			errors.push(`${where}: min checked must be between 1 and the item count`);
		}
		checkOutcome(outcome, `${where} checklist`);
	}
	for (const screen of library.screens) {
		const targets = [
			...(screen.options ?? []).flatMap((option) =>
				option.outcome.type === 'screen' ? [option.outcome.screenId] : [],
			),
			...(screen.checklist?.outcome.type === 'screen' ? [screen.checklist.outcome.screenId] : []),
			...(screen.nextScreenId !== undefined ? [screen.nextScreenId] : []),
		];
		for (const next of targets) {
			if (!screenIds.has(next)) {
				errors.push(`screen ${screen.id}: next screen ${next} is not defined`);
			}
		}
	}
	for (const {target, surface} of REPORT_FLOW_VARIANTS) {
		const start = library.flows[target].start[surface];
		if (start !== undefined && !screenIds.has(start)) {
			errors.push(`flow ${target}/${surface}: start screen ${start} is not defined`);
		}
	}
	return errors;
}

function collectVariantErrors(variant: ReportFlowVariant): Array<string> {
	const errors: Array<string> = [];
	const name = `flow ${variant.target}/${variant.surface}`;
	const startKind = variant.screens.get(variant.startScreenId)?.kind;
	const mustStartWithInfo = variant.target === 'user' && variant.surface === 'in_app';
	if (mustStartWithInfo !== (startKind === 'info')) {
		errors.push(
			mustStartWithInfo
				? `${name}: the start screen must be an info screen`
				: `${name}: the start screen must not be an info screen`,
		);
	}
	for (const screen of variant.screens.values()) {
		if (screen.kind === 'choice' && !screen.options.some((option) => option.outcome.type !== 'link')) {
			errors.push(`${name} screen ${screen.def.id}: no option that is not a link remains`);
		}
		for (const outcome of getScreenOutcomes(screen)) {
			if (outcome.type === 'submit') {
				checkLegacyCategory(outcome.reason, variant.target, `${name} screen ${screen.def.id}`, errors);
			}
		}
		for (const item of screen.def.checklist?.items ?? []) {
			if (item.reason !== undefined) {
				checkLegacyCategory(item.reason, variant.target, `${name} screen ${screen.def.id}`, errors);
			}
		}
	}
	const depth = new Map<string, number>();
	const visiting = new Set<string>();
	const measure = (screenId: string): number => {
		const known = depth.get(screenId);
		if (known !== undefined) {
			return known;
		}
		if (visiting.has(screenId)) {
			errors.push(`${name}: cycle through screen ${screenId}`);
			return Number.POSITIVE_INFINITY;
		}
		visiting.add(screenId);
		const screen = variant.screens.get(screenId);
		let longest = 0;
		for (const next of screen ? getScreenEdges(screen) : []) {
			longest = Math.max(longest, measure(next));
		}
		visiting.delete(screenId);
		depth.set(screenId, longest + 1);
		return longest + 1;
	};
	const longestWalk = measure(variant.startScreenId);
	if (Number.isFinite(longestWalk) && longestWalk > MAX_WALK_SCREENS) {
		errors.push(`${name}: longest walk is ${longestWalk} screens, the limit is ${MAX_WALK_SCREENS}`);
	}
	return errors;
}

function checkLegacyCategory(reason: string, target: ReportFlowTargetType, where: string, errors: Array<string>) {
	if (!isReportReasonKey(reason) || !getLegacyCategory(reason, target)) {
		errors.push(`${where}: reason ${reason} has no legacy category for ${target}`);
	}
}

function assertNoDefinitionErrors(errors: Array<string>): void {
	if (errors.length > 0) {
		throw new Error(`Invalid report flow definitions:\n${[...new Set(errors)].join('\n')}`);
	}
}

export function assertValidReportFlowLibrary(library: ReportFlowLibrary): void {
	const errors = collectDefinitionErrors(library);
	assertNoDefinitionErrors(errors);
	const reachable = new Set<string>();
	for (const instance of REPORT_FLOW_CHECKED_INSTANCES) {
		for (const {target, surface} of REPORT_FLOW_VARIANTS) {
			const variant = buildReportFlowVariant(library, target, surface, instance);
			if (!variant) {
				continue;
			}
			errors.push(...collectVariantErrors(variant));
			for (const screenId of variant.screens.keys()) {
				reachable.add(screenId);
			}
		}
	}
	for (const screen of library.screens) {
		if (!reachable.has(screen.id)) {
			errors.push(`screen ${screen.id}: not reachable from any flow`);
		}
	}
	assertNoDefinitionErrors(errors);
}
