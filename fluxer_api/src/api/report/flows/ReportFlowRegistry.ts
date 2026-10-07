// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {Config} from '@app/api/Config';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import {getLegalUrls} from '@app/api/instance/LegalUrls';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {REPORT_FLOWS, type ReportFlowDef} from '@app/api/report/flows/ReportFlowDefinitions';
import {
	REPORT_FLOW_NOTICES,
	REPORT_FLOW_SCREENS,
	type ReportFlowCopyKey,
	type ReportFlowLinkId,
	type ReportFlowNoticeDef,
	type ReportFlowOptionDef,
	type ReportFlowOutcomeDef,
	type ReportFlowScreenDef,
} from '@app/api/report/flows/ReportFlowScreens';
import {getLegacyCategory, isReportReasonKey, type ReportReasonKey} from '@app/api/report/flows/ReportReasonCatalog';
import type {LocaleCode} from '@fluxer/constants/src/Locales';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InvalidReportFlowAnswersError} from '@fluxer/errors/src/domains/moderation/InvalidReportFlowAnswersError';
import {ReportFlowOutdatedError} from '@fluxer/errors/src/domains/moderation/ReportFlowOutdatedError';
import type {
	ReportFlowOutcome,
	ReportFlowResponse,
	ReportFlowScreen,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {parseAcceptLanguage} from '@pkgs/locale/src/LocaleService';

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

interface ReportFlowVariantScreen {
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

export interface ReportFlowStepRecord {
	screen_id: string;
	option_id?: string;
	item_ids?: Array<string>;
}

export interface ReportFlowStepInput {
	screen_id: string;
	option_id?: string | undefined;
	item_ids?: ReadonlyArray<string> | undefined;
}

export interface ResolvedReportFlowAnswers {
	reason: ReportReasonKey;
	legacyCategory: string;
	steps: Array<ReportFlowStepRecord>;
	stepsJson: string;
	currentRevisionHash: string;
	isCurrentRevision: boolean;
}

export interface ReportFlowAnswersDescription {
	revision_hash: string;
	surface: string;
	locale: string | null;
	steps: Array<{
		screen_id: string;
		screen_title: string;
		option_id: string | null;
		option_label: string | null;
		items: Array<{id: string; label: string}>;
	}>;
}

export const REPORT_FLOW_LIBRARY: ReportFlowLibrary = {
	flows: REPORT_FLOWS,
	screens: REPORT_FLOW_SCREENS,
	notices: REPORT_FLOW_NOTICES,
};

const REPORT_FLOW_TARGETS: ReadonlyArray<ReportFlowTargetType> = ['message', 'user', 'guild'];
const REPORT_FLOW_SURFACES: ReadonlyArray<ReportFlowSurface> = ['in_app', 'dsa'];
const REPORT_FLOW_ID_PATTERN = /^[a-z0-9_]{1,48}$/;
const MAX_WALK_SCREENS = 10;
const CRISIS_LINES_URL = 'https://befrienders.org';
const HOSTED_REPORT_FLOW_INSTANCE: ReportFlowInstance = {selfHosted: false, guidelinesLinked: true};
const REPORT_FLOW_CHECKED_INSTANCES: ReadonlyArray<ReportFlowInstance> = [
	HOSTED_REPORT_FLOW_INSTANCE,
	{selfHosted: true, guidelinesLinked: false},
	{selfHosted: true, guidelinesLinked: true},
];
const ADMIN_LOCALE = 'en-US';

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
		case 'dsa':
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
			.sort((a, b) => (a.def.id < b.def.id ? -1 : a.def.id > b.def.id ? 1 : 0))
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
			const linksToDsa =
				option.outcome.type === 'link' && (option.outcome.link === 'dsa' || option.outcome.link === 'copyright');
			if ((endsWithoutNotice || linksToDsa) && option.surface !== 'in_app') {
				errors.push(`${optionWhere}: a no-report or DSA link option must be limited to in_app`);
			}
		}
		if (screen.checklist) {
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
	for (const target of REPORT_FLOW_TARGETS) {
		for (const surface of REPORT_FLOW_SURFACES) {
			const start = library.flows[target].start[surface];
			if (start !== undefined && !screenIds.has(start)) {
				errors.push(`flow ${target}/${surface}: start screen ${start} is not defined`);
			}
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

export function assertValidReportFlowLibrary(library: ReportFlowLibrary): void {
	const errors = collectDefinitionErrors(library);
	if (errors.length === 0) {
		const reachable = new Set<string>();
		for (const instance of REPORT_FLOW_CHECKED_INSTANCES) {
			for (const target of REPORT_FLOW_TARGETS) {
				for (const surface of REPORT_FLOW_SURFACES) {
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
		}
		for (const screen of library.screens) {
			if (!reachable.has(screen.id)) {
				errors.push(`screen ${screen.id}: not reachable from any flow`);
			}
		}
	}
	if (errors.length > 0) {
		throw new Error(`Invalid report flow definitions:\n${[...new Set(errors)].join('\n')}`);
	}
}

export function buildReportFlowLedger(library: ReportFlowLibrary): Record<string, string> {
	const ledger: Record<string, string> = {};
	for (const target of REPORT_FLOW_TARGETS) {
		for (const surface of REPORT_FLOW_SURFACES) {
			const variant = buildReportFlowVariant(library, target, surface, HOSTED_REPORT_FLOW_INSTANCE);
			if (!variant) {
				continue;
			}
			for (const screen of variant.screens.values()) {
				const prefix = `${target}/${screen.def.id}`;
				for (const option of screen.options) {
					ledger[`${prefix}/${option.id}`] = canonicalOutcome(option.outcome);
				}
				if (screen.def.checklist) {
					for (const item of screen.def.checklist.items) {
						ledger[`${prefix}/${item.id}`] = item.reason === undefined ? 'item' : `item:${item.reason}`;
					}
					ledger[`${prefix}/#next`] = canonicalOutcome(screen.def.checklist.outcome);
				}
				if (screen.def.nextScreenId !== undefined) {
					ledger[`${prefix}/#next`] = `screen:${screen.def.nextScreenId}`;
				}
			}
		}
	}
	return Object.fromEntries(Object.entries(ledger).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)));
}

assertValidReportFlowLibrary(REPORT_FLOW_LIBRARY);

const variantCache = new Map<string, ReportFlowVariant | null>();
const responseCache = new Map<string, ReportFlowResponse>();

function getReportFlowInstance(guidelinesUrl: string | null): ReportFlowInstance {
	return {selfHosted: Config.instance.selfHosted, guidelinesLinked: guidelinesUrl !== null};
}

function getLinkUrl(link: ReportFlowLinkId, guidelinesUrl: string | null): string | null {
	switch (link) {
		case 'dsa':
			return `${Config.endpoints.webApp}/report`;
		case 'copyright':
			return `${Config.endpoints.webApp}/report?option=copyright_notice`;
		case 'crisis_lines':
			return CRISIS_LINES_URL;
		case 'guidelines':
			return guidelinesUrl;
	}
}

function findReportFlowVariant(
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	instance: ReportFlowInstance,
): ReportFlowVariant | null {
	const cacheKey = `${target}:${surface}:${instance.selfHosted}:${instance.guidelinesLinked}`;
	if (!variantCache.has(cacheKey)) {
		variantCache.set(cacheKey, buildReportFlowVariant(REPORT_FLOW_LIBRARY, target, surface, instance));
	}
	return variantCache.get(cacheKey) ?? null;
}

function requireReportFlowVariant(
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	instance: ReportFlowInstance,
): ReportFlowVariant {
	const variant = findReportFlowVariant(target, surface, instance);
	if (!variant) {
		throw InputValidationError.fromCode('surface', ValidationErrorCodes.INVALID_FORMAT);
	}
	return variant;
}

export function getReportFlowVariant(target: ReportFlowTargetType, surface: ReportFlowSurface): ReportFlowVariant {
	return requireReportFlowVariant(target, surface, getReportFlowInstance(getLegalUrls().guidelinesUrl));
}

export function resolveReportFlowLocale(raw: string | null | undefined): LocaleCode {
	return parseAcceptLanguage(raw);
}

function renderCopy(key: ReportFlowCopyKey, locale: string): string {
	return getContentMessage(key, locale, {product_name: getInstanceProductName()});
}

function renderOutcome(outcome: ReportFlowOutcomeDef, guidelinesUrl: string | null): ReportFlowOutcome {
	return {
		type: outcome.type,
		screen_id: outcome.type === 'screen' ? outcome.screenId : null,
		reason: outcome.type === 'submit' ? outcome.reason : null,
		notice_id: outcome.type === 'end' ? (outcome.noticeId ?? null) : null,
		url: outcome.type === 'link' ? getLinkUrl(outcome.link, guidelinesUrl) : null,
	};
}

function renderScreen(screen: ReportFlowVariantScreen, locale: string, guidelinesUrl: string | null): ReportFlowScreen {
	const {def} = screen;
	return {
		id: def.id,
		title: renderCopy(def.title, locale),
		subtitle: screen.subtitle ? renderCopy(screen.subtitle, locale) : null,
		urgent: def.urgent === true,
		options: screen.options.map((option) => ({
			id: option.id,
			label: renderCopy(option.label, locale),
			outcome: renderOutcome(option.outcome, guidelinesUrl),
		})),
		options_heading: def.optionsHeading && screen.options.length > 0 ? renderCopy(def.optionsHeading, locale) : null,
		checklist: def.checklist
			? {
					items: def.checklist.items.map((item) => ({
						id: item.id,
						label: renderCopy(item.label, locale),
						description: item.description ? renderCopy(item.description, locale) : null,
					})),
					min_checked: def.checklist.minChecked,
					outcome: renderOutcome(def.checklist.outcome, guidelinesUrl),
				}
			: null,
		next_screen_id: def.nextScreenId ?? null,
	};
}

export function getReportFlowResponse(
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	rawLocale: string | null | undefined,
): ReportFlowResponse {
	const {guidelinesUrl} = getLegalUrls();
	const instance = getReportFlowInstance(guidelinesUrl);
	const variant = requireReportFlowVariant(target, surface, instance);
	const locale = resolveReportFlowLocale(rawLocale);
	const cacheKey = [
		target,
		surface,
		locale,
		instance.selfHosted,
		guidelinesUrl ?? '',
		getInstanceProductName(),
		Config.endpoints.webApp,
	].join('\u0000');
	const cached = responseCache.get(cacheKey);
	if (cached) {
		return cached;
	}
	const response: ReportFlowResponse = {
		target_type: target,
		surface,
		revision_hash: variant.revisionHash,
		locale,
		start_screen_id: variant.startScreenId,
		guidelines_url: guidelinesUrl,
		screens: [...variant.screens.values()].map((screen) => renderScreen(screen, locale, guidelinesUrl)),
		notices: variant.notices.map((notice) => ({
			id: notice.id,
			title: renderCopy(notice.title, locale),
			body: renderCopy(notice.body, locale),
		})),
	};
	responseCache.set(cacheKey, response);
	return response;
}

function findInvalidStep(
	variant: ReportFlowVariant,
	steps: ReadonlyArray<ReportFlowStepInput>,
): {invalidStep: number} | {reason: ReportReasonKey; steps: Array<ReportFlowStepRecord>} {
	let expectedScreenId: string | null = variant.startScreenId;
	let finalReason: ReportReasonKey | null = null;
	let itemReason: ReportReasonKey | null = null;
	const records: Array<ReportFlowStepRecord> = [];
	for (let index = 0; index < steps.length; index++) {
		const step = steps[index];
		if (expectedScreenId === null || step.screen_id !== expectedScreenId) {
			return {invalidStep: index};
		}
		const screen = variant.screens.get(expectedScreenId);
		if (!screen) {
			return {invalidStep: index};
		}
		let outcome: ReportFlowOutcomeDef;
		if (screen.kind === 'choice') {
			const option =
				step.option_id !== undefined && step.item_ids === undefined
					? screen.options.find((candidate) => candidate.id === step.option_id)
					: undefined;
			if (!option) {
				return {invalidStep: index};
			}
			outcome = option.outcome;
			records.push({screen_id: step.screen_id, option_id: option.id});
		} else if (screen.kind === 'checklist' && screen.def.checklist) {
			const {items, minChecked} = screen.def.checklist;
			const chosen = new Set(step.item_ids ?? []);
			const known = items.filter((item) => chosen.has(item.id));
			if (
				step.option_id !== undefined ||
				step.item_ids === undefined ||
				chosen.size !== step.item_ids.length ||
				known.length !== chosen.size ||
				known.length < minChecked
			) {
				return {invalidStep: index};
			}
			itemReason ??= known.find((item) => item.reason !== undefined)?.reason ?? null;
			outcome = screen.def.checklist.outcome;
			records.push({screen_id: step.screen_id, item_ids: known.map((item) => item.id)});
		} else {
			if (step.option_id !== undefined || step.item_ids !== undefined || screen.def.nextScreenId === undefined) {
				return {invalidStep: index};
			}
			outcome = {type: 'screen', screenId: screen.def.nextScreenId};
			records.push({screen_id: step.screen_id});
		}
		if (outcome.type === 'screen') {
			expectedScreenId = outcome.screenId;
		} else if (outcome.type === 'submit') {
			expectedScreenId = null;
			finalReason = outcome.reason;
		} else {
			return {invalidStep: index};
		}
	}
	if (finalReason === null) {
		return {invalidStep: Math.max(steps.length - 1, 0)};
	}
	return {reason: itemReason ?? finalReason, steps: records};
}

export function resolveReportFlowAnswers(params: {
	target: ReportFlowTargetType;
	surface: ReportFlowSurface;
	revisionHash: string;
	steps: ReadonlyArray<ReportFlowStepInput>;
}): ResolvedReportFlowAnswers {
	const variant = getReportFlowVariant(params.target, params.surface);
	const isCurrentRevision = params.revisionHash === variant.revisionHash;
	const result = findInvalidStep(variant, params.steps);
	if ('invalidStep' in result) {
		if (isCurrentRevision) {
			throw new InvalidReportFlowAnswersError(result.invalidStep);
		}
		throw new ReportFlowOutdatedError();
	}
	return {
		reason: result.reason,
		legacyCategory: getLegacyCategory(result.reason, params.target),
		steps: result.steps,
		stepsJson: JSON.stringify(result.steps),
		currentRevisionHash: variant.revisionHash,
		isCurrentRevision,
	};
}

function isStringArray(value: unknown): value is Array<string> {
	return Array.isArray(value) && value.every((entry) => typeof entry === 'string');
}

export function parseReportFlowSteps(json: string | null | undefined): Array<ReportFlowStepRecord> | null {
	if (!json) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(json);
	} catch {
		return null;
	}
	if (!Array.isArray(parsed)) {
		return null;
	}
	const steps: Array<ReportFlowStepRecord> = [];
	for (const entry of parsed) {
		if (typeof entry !== 'object' || entry === null) {
			return null;
		}
		const {screen_id, option_id, item_ids} = entry as Record<string, unknown>;
		if (
			typeof screen_id !== 'string' ||
			(option_id !== undefined && typeof option_id !== 'string') ||
			(item_ids !== undefined && !isStringArray(item_ids))
		) {
			return null;
		}
		steps.push({
			screen_id,
			...(option_id !== undefined ? {option_id} : {}),
			...(item_ids !== undefined ? {item_ids} : {}),
		});
	}
	return steps;
}

export function describeReportFlowAnswers(params: {
	revisionHash: string;
	surface: string;
	locale: string | null;
	steps: ReadonlyArray<ReportFlowStepRecord>;
}): ReportFlowAnswersDescription {
	const screenDefs = new Map(REPORT_FLOW_LIBRARY.screens.map((screen) => [screen.id, screen]));
	return {
		revision_hash: params.revisionHash,
		surface: params.surface,
		locale: params.locale,
		steps: params.steps.map((step) => {
			const screen = screenDefs.get(step.screen_id);
			const option =
				step.option_id === undefined
					? undefined
					: screen?.options?.find((candidate) => candidate.id === step.option_id);
			return {
				screen_id: step.screen_id,
				screen_title: screen ? renderCopy(screen.title, ADMIN_LOCALE) : step.screen_id,
				option_id: step.option_id ?? null,
				option_label:
					step.option_id === undefined ? null : option ? renderCopy(option.label, ADMIN_LOCALE) : step.option_id,
				items: (step.item_ids ?? []).map((itemId) => {
					const item = screen?.checklist?.items.find((candidate) => candidate.id === itemId);
					return {id: itemId, label: item ? renderCopy(item.label, ADMIN_LOCALE) : itemId};
				}),
			};
		}),
	};
}
