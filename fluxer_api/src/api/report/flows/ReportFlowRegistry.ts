// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {getContentMessage} from '@app/api/content_i18n/ContentI18n';
import {getLegalUrls} from '@app/api/instance/LegalUrls';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {
	assertValidReportFlowLibrary,
	buildReportFlowVariant,
	type ReportFlowInstance,
	type ReportFlowLibrary,
	type ReportFlowVariant,
	type ReportFlowVariantScreen,
} from '@app/api/report/flows/ReportFlowCompiler';
import {REPORT_FLOWS} from '@app/api/report/flows/ReportFlowDefinitions';
import {
	REPORT_FLOW_NOTICES,
	REPORT_FLOW_SCREENS,
	type ReportFlowCopyKey,
	type ReportFlowLinkId,
	type ReportFlowOutcomeDef,
} from '@app/api/report/flows/ReportFlowScreens';
import {getLegacyCategory, type ReportReasonKey} from '@app/api/report/flows/ReportReasonCatalog';
import {isStringArray, parseJsonArray} from '@app/api/utils/JsonBoundaryUtils';
import type {LocaleCode} from '@fluxer/constants/src/Locales';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {InvalidReportFlowAnswersError} from '@fluxer/errors/src/domains/moderation/InvalidReportFlowAnswersError';
import {ReportFlowOutdatedError} from '@fluxer/errors/src/domains/moderation/ReportFlowOutdatedError';
import {parseAcceptLanguage} from '@fluxer/i18n/src/locale/resolution/AcceptLanguageNegotiation';
import {isObjectLike} from '@fluxer/record_utils/src/RecordGuards';
import type {
	ReportFlowOutcome,
	ReportFlowResponse,
	ReportFlowScreen,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

export interface ReportFlowStepRecord {
	screen_id: string;
	option_id?: string;
	item_ids?: Array<string>;
}

interface ReportFlowStepInput {
	screen_id: string;
	option_id?: string | undefined;
	item_ids?: ReadonlyArray<string> | undefined;
}

interface ResolvedReportFlowAnswers {
	reason: ReportReasonKey;
	legacyCategory: string;
	steps: Array<ReportFlowStepRecord>;
	stepsJson: string;
	currentRevisionHash: string;
	isCurrentRevision: boolean;
}

interface ReportFlowAnswersDescription {
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

const REPORT_FLOW_LIBRARY: ReportFlowLibrary = {
	flows: REPORT_FLOWS,
	screens: REPORT_FLOW_SCREENS,
	notices: REPORT_FLOW_NOTICES,
};

const CRISIS_LINES_URL = 'https://befrienders.org';
const COPYRIGHT_HELP_URL = 'https://fluxer.com/help/copyright';
const ADMIN_LOCALE = 'en-US';

assertValidReportFlowLibrary(REPORT_FLOW_LIBRARY);

const variantCache = new Map<string, ReportFlowVariant | null>();
const responseCache = new Map<string, ReportFlowResponse>();

function getReportFlowInstance(guidelinesUrl: string | null): ReportFlowInstance {
	return {selfHosted: Config.instance.selfHosted, guidelinesLinked: guidelinesUrl !== null};
}

function getLinkUrl(link: ReportFlowLinkId, guidelinesUrl: string | null): string | null {
	switch (link) {
		case 'copyright':
			return COPYRIGHT_HELP_URL;
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
	const variant = requireReportFlowVariant(
		params.target,
		params.surface,
		getReportFlowInstance(getLegalUrls().guidelinesUrl),
	);
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

export function parseReportFlowSteps(json: string | null | undefined): Array<ReportFlowStepRecord> | null {
	if (!json) {
		return null;
	}
	const parsed = parseJsonArray(json);
	if (!parsed) {
		return null;
	}
	const steps: Array<ReportFlowStepRecord> = [];
	for (const entry of parsed) {
		if (!isObjectLike(entry)) {
			return null;
		}
		const {screen_id, option_id, item_ids} = entry;
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
			const screenTitle = screen ? renderCopy(screen.title, ADMIN_LOCALE) : step.screen_id;
			let optionLabel: string | null = null;
			if (step.option_id !== undefined) {
				optionLabel = option ? renderCopy(option.label, ADMIN_LOCALE) : step.option_id;
			}
			return {
				screen_id: step.screen_id,
				screen_title: screenTitle,
				option_id: step.option_id ?? null,
				option_label: optionLabel,
				items: (step.item_ids ?? []).map((itemId) => {
					const item = screen?.checklist?.items.find((candidate) => candidate.id === itemId);
					return {id: itemId, label: item ? renderCopy(item.label, ADMIN_LOCALE) : itemId};
				}),
			};
		}),
	};
}
