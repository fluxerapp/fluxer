// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {getConfig} from '@app/api/Config';
import {CONTENT_I18N_MESSAGES} from '@app/api/content_i18n/ContentI18nMessages';
import {
	assertValidReportFlowLibrary,
	buildReportFlowLedger,
	buildReportFlowVariant,
	describeReportFlowAnswers,
	getReportFlowResponse,
	getReportFlowVariant,
	parseReportFlowSteps,
	REPORT_FLOW_LIBRARY,
	type ReportFlowLibrary,
	type ReportFlowStepInput,
	type ReportFlowVariant,
	resolveReportFlowAnswers,
	resolveReportFlowLocale,
} from '@app/api/report/flows/ReportFlowRegistry';
import type {ReportFlowOptionDef, ReportFlowScreenDef} from '@app/api/report/flows/ReportFlowScreens';
import {
	findReportReason,
	getLegacyCategory,
	listReportReasons,
	REPORT_REASONS,
} from '@app/api/report/flows/ReportReasonCatalog';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {
	ReportFlowResponse,
	type ReportFlowSurface,
	type ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	ReportGuildRequest,
	ReportMessageRequest,
	ReportUserRequest,
} from '@fluxer/schema/src/domains/report/ReportSchemas';
import {afterEach, describe, expect, test} from 'vitest';

const THIS_DIR = path.dirname(fileURLToPath(import.meta.url));
const LEDGER_PATH = path.join(THIS_DIR, '../flows/ReportFlowIds.snapshot.json');

const VARIANTS: ReadonlyArray<[ReportFlowTargetType, ReportFlowSurface]> = [
	['message', 'in_app'],
	['message', 'dsa'],
	['user', 'in_app'],
	['user', 'dsa'],
	['guild', 'dsa'],
];

interface Ledger {
	active: Record<string, string>;
	retired: Record<string, string>;
}

function readLedger(): Ledger {
	return JSON.parse(fs.readFileSync(LEDGER_PATH, 'utf8')) as Ledger;
}

function findLedgerViolations(ledger: Ledger, current: Record<string, string>): Array<string> {
	const violations: Array<string> = [];
	for (const [id, value] of Object.entries(ledger.active)) {
		if (!Object.hasOwn(current, id)) {
			violations.push(`${id} disappeared without moving to retired`);
		} else if (current[id] !== value) {
			violations.push(`${id} changed from ${value} to ${current[id]}`);
		}
	}
	for (const id of Object.keys(current)) {
		if (!Object.hasOwn(ledger.active, id)) {
			violations.push(`${id} is missing from active`);
		}
		if (Object.hasOwn(ledger.retired, id)) {
			violations.push(`${id} is retired and cannot be reused`);
		}
	}
	return violations;
}

function editScreen(
	library: ReportFlowLibrary,
	screenId: string,
	edit: (screen: ReportFlowScreenDef) => ReportFlowScreenDef,
): ReportFlowLibrary {
	expect(library.screens.some((screen) => screen.id === screenId)).toBe(true);
	return {
		...library,
		screens: library.screens.map((screen) => (screen.id === screenId ? edit(screen) : screen)),
	};
}

function addOption(library: ReportFlowLibrary, screenId: string, option: ReportFlowOptionDef): ReportFlowLibrary {
	return editScreen(library, screenId, (screen) => ({...screen, options: [...(screen.options ?? []), option]}));
}

function variantOf(
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	library: ReportFlowLibrary = REPORT_FLOW_LIBRARY,
	selfHosted = false,
): ReportFlowVariant {
	const variant = buildReportFlowVariant(library, target, surface, {selfHosted});
	if (!variant) {
		throw new Error(`no variant ${target}/${surface}`);
	}
	return variant;
}

function variantReasons(variant: ReportFlowVariant): Set<string> {
	const reasons = new Set<string>();
	for (const screen of variant.screens.values()) {
		for (const option of screen.options) {
			if (option.outcome.type === 'submit') {
				reasons.add(option.outcome.reason);
			}
		}
		if (screen.def.checklist?.outcome.type === 'submit') {
			reasons.add(screen.def.checklist.outcome.reason);
		}
		for (const item of screen.def.checklist?.items ?? []) {
			if (item.reason) {
				reasons.add(item.reason);
			}
		}
	}
	return reasons;
}

function optionIds(variant: ReportFlowVariant, screenId: string): Array<string> {
	const screen = variant.screens.get(screenId);
	expect(screen).toBeDefined();
	return screen!.options.map((option) => option.id);
}

function resolve(target: ReportFlowTargetType, surface: ReportFlowSurface, steps: ReadonlyArray<ReportFlowStepInput>) {
	return resolveReportFlowAnswers({
		target,
		surface,
		revisionHash: getReportFlowVariant(target, surface).revisionHash,
		steps,
	});
}

function captureError(run: () => unknown): FluxerError {
	try {
		run();
	} catch (error) {
		if (error instanceof FluxerError) {
			return error;
		}
		throw error;
	}
	throw new Error('expected an error');
}

function expectInvalidStep(
	target: ReportFlowTargetType,
	surface: ReportFlowSurface,
	steps: ReadonlyArray<ReportFlowStepInput>,
	stepIndex: number,
) {
	const error = captureError(() => resolve(target, surface, steps));
	expect(error.code).toBe(APIErrorCodes.INVALID_REPORT_FLOW_ANSWERS);
	expect(error.status).toBe(400);
	expect(error.data).toEqual({step_index: stepIndex});
}

const originalSelfHosted = getConfig().instance.selfHosted;

afterEach(() => {
	getConfig().instance.selfHosted = originalSelfHosted;
});

describe('report flow definitions', () => {
	test('the shipped library passes every rule on every variant', () => {
		expect(() => assertValidReportFlowLibrary(REPORT_FLOW_LIBRARY)).not.toThrow();
		for (const selfHosted of [false, true]) {
			for (const [target, surface] of VARIANTS) {
				const variant = variantOf(target, surface, REPORT_FLOW_LIBRARY, selfHosted);
				for (const screen of variant.screens.values()) {
					const hasOptions = (screen.def.options ?? []).length > 0;
					if (screen.kind === 'checklist') {
						expect(hasOptions || screen.def.nextScreenId !== undefined).toBe(false);
					}
					if (screen.kind === 'info') {
						expect(screen.options.every((option) => option.outcome.type === 'link')).toBe(true);
					}
					if (screen.kind === 'choice') {
						expect(screen.options.some((option) => option.outcome.type !== 'link')).toBe(true);
					}
					for (const option of screen.options) {
						expect(option.id).toMatch(/^[a-z0-9_]{1,48}$/);
						if (surface === 'dsa') {
							expect(option.surface).not.toBe('in_app');
							expect(option.outcome.type === 'end' && option.outcome.noticeId === undefined).toBe(false);
						}
					}
				}
				expect(variant.screens.get(variant.startScreenId)?.kind === 'info').toBe(
					target === 'user' && surface === 'in_app',
				);
			}
		}
		expect(buildReportFlowVariant(REPORT_FLOW_LIBRARY, 'guild', 'in_app', {selfHosted: false})).toBeNull();
	});

	test('every screen in the library is used by a variant', () => {
		const used = new Set<string>();
		for (const [target, surface] of VARIANTS) {
			for (const screenId of variantOf(target, surface).screens.keys()) {
				used.add(screenId);
			}
		}
		expect([...used].sort()).toEqual(REPORT_FLOW_LIBRARY.screens.map((screen) => screen.id).sort());
	});

	test('a cycle throws', () => {
		const broken = addOption(REPORT_FLOW_LIBRARY, 'harassment', {
			id: 'loop',
			label: 'report_flow.label.abuse',
			outcome: {type: 'screen', screenId: 'root_message'},
		});
		expect(() => assertValidReportFlowLibrary(broken)).toThrow(/cycle/);
	});

	test('a dangling screen reference throws', () => {
		const broken = addOption(REPORT_FLOW_LIBRARY, 'abuse', {
			id: 'nowhere',
			label: 'report_flow.label.abuse',
			outcome: {type: 'screen', screenId: 'missing_screen'},
		});
		expect(() => assertValidReportFlowLibrary(broken)).toThrow(/missing_screen is not defined/);
	});

	test('a no-report or DSA option that is not limited to the app throws', () => {
		const unlimitedDislike = editScreen(REPORT_FLOW_LIBRARY, 'root_message', (screen) => ({
			...screen,
			options: screen.options?.map((option) =>
				option.id === 'dislike' ? {id: option.id, label: option.label, outcome: option.outcome} : option,
			),
		}));
		expect(() => assertValidReportFlowLibrary(unlimitedDislike)).toThrow(/limited to in_app/);
		const unlimitedDsaRow = editScreen(REPORT_FLOW_LIBRARY, 'root_user', (screen) => ({
			...screen,
			options: screen.options?.map((option) => (option.id === 'dsa' ? {...option, surface: undefined} : option)),
		}));
		expect(() => assertValidReportFlowLibrary(unlimitedDsaRow)).toThrow(/limited to in_app/);
	});

	test('other broken definitions throw', () => {
		const unreachable: ReportFlowLibrary = {
			...REPORT_FLOW_LIBRARY,
			screens: [
				...REPORT_FLOW_LIBRARY.screens,
				{
					id: 'orphan',
					title: 'report_flow.screen.abuse.title',
					options: [{id: 'spam', label: 'report_flow.label.spam', outcome: {type: 'submit', reason: 'spam'}}],
				},
			],
		};
		expect(() => assertValidReportFlowLibrary(unreachable)).toThrow(/orphan: not reachable/);
		const duplicateOption = addOption(REPORT_FLOW_LIBRARY, 'abuse', {
			id: 'hate',
			label: 'report_flow.label.hate',
			outcome: {type: 'screen', screenId: 'hate'},
		});
		expect(() => assertValidReportFlowLibrary(duplicateOption)).toThrow(/duplicate option id/);
		const badId = addOption(REPORT_FLOW_LIBRARY, 'abuse', {
			id: 'Bad-Id',
			label: 'report_flow.label.spam',
			outcome: {type: 'submit', reason: 'spam'},
		});
		expect(() => assertValidReportFlowLibrary(badId)).toThrow(/does not match/);
		const badMinimum = editScreen(REPORT_FLOW_LIBRARY, 'private_info', (screen) => ({
			...screen,
			checklist: screen.checklist ? {...screen.checklist, minChecked: 0} : undefined,
		}));
		expect(() => assertValidReportFlowLibrary(badMinimum)).toThrow(/min checked/);
		const mixedKinds = editScreen(REPORT_FLOW_LIBRARY, 'crisis_support', (screen) => ({
			...screen,
			options: [{id: 'spam', label: 'report_flow.label.spam', outcome: {type: 'submit', reason: 'spam'}}],
		}));
		expect(() => assertValidReportFlowLibrary(mixedKinds)).toThrow(/only has link options/);
		const infoStart: ReportFlowLibrary = {
			...REPORT_FLOW_LIBRARY,
			flows: {
				...REPORT_FLOW_LIBRARY.flows,
				message: {target: 'message', start: {in_app: 'crisis_support', dsa: 'root_message'}},
			},
		};
		expect(() => assertValidReportFlowLibrary(infoStart)).toThrow(/must not be an info screen/);
		const unknownReason = addOption(REPORT_FLOW_LIBRARY, 'abuse', {
			id: 'invented',
			label: 'report_flow.label.spam',
			outcome: {type: 'submit', reason: 'invented' as 'spam'},
		});
		expect(() => assertValidReportFlowLibrary(unknownReason)).toThrow(/unknown reason invented/);
	});
});

describe('report flow id ledger', () => {
	test('the ledger matches the current definitions', () => {
		const ledger = readLedger();
		expect(findLedgerViolations(ledger, buildReportFlowLedger(REPORT_FLOW_LIBRARY))).toEqual([]);
		expect(Object.keys(ledger.active).length).toBeGreaterThan(200);
	});

	test('the ledger catches changed, removed, added and reused ids', () => {
		const ledger = readLedger();
		const changed = editScreen(REPORT_FLOW_LIBRARY, 'root_message', (screen) => ({
			...screen,
			options: screen.options?.map((option) =>
				option.id === 'spam' ? {...option, outcome: {type: 'submit', reason: 'fake_account'}} : option,
			),
		}));
		expect(findLedgerViolations(ledger, buildReportFlowLedger(changed))).toContain(
			'message/root_message/spam changed from submit:spam to submit:fake_account',
		);
		const removed = editScreen(REPORT_FLOW_LIBRARY, 'impersonation', (screen) => ({
			...screen,
			options: screen.options?.filter((option) => option.id !== 'fraud'),
		}));
		expect(findLedgerViolations(ledger, buildReportFlowLedger(removed))).toContain(
			'message/impersonation/fraud disappeared without moving to retired',
		);
		const added = addOption(REPORT_FLOW_LIBRARY, 'impersonation', {
			id: 'impersonation_bot',
			label: 'report_flow.label.fake_account',
			outcome: {type: 'submit', reason: 'fake_account'},
		});
		expect(findLedgerViolations(ledger, buildReportFlowLedger(added))).toContain(
			'message/impersonation/impersonation_bot is missing from active',
		);
		const reused: Ledger = {
			active: ledger.active,
			retired: {'message/root_message/spam': 'submit:spam'},
		};
		expect(findLedgerViolations(reused, buildReportFlowLedger(REPORT_FLOW_LIBRARY))).toContain(
			'message/root_message/spam is retired and cannot be reused',
		);
		const checklistOutcome = editScreen(REPORT_FLOW_LIBRARY, 'private_info', (screen) => ({
			...screen,
			checklist: screen.checklist ? {...screen.checklist, outcome: {type: 'submit', reason: 'other'}} : undefined,
		}));
		expect(findLedgerViolations(ledger, buildReportFlowLedger(checklistOutcome))).toContain(
			'message/private_info/#next changed from submit:doxxing to submit:other',
		);
	});
});

describe('report flow walks', () => {
	const walks: ReadonlyArray<{
		name: string;
		target: ReportFlowTargetType;
		surface?: ReportFlowSurface;
		steps: ReadonlyArray<ReportFlowStepInput>;
		reason: string;
		category: string;
	}> = [
		{
			name: 'S3 spam',
			target: 'message',
			steps: [{screen_id: 'root_message', option_id: 'spam'}],
			reason: 'spam',
			category: 'spam',
		},
		{
			name: 'S6 private information',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'private_info'},
				{screen_id: 'private_info', item_ids: ['phone', 'email']},
			],
			reason: 'doxxing',
			category: 'doxxing',
		},
		{
			name: 'S6 intimate photo override',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'private_info'},
				{screen_id: 'private_info', item_ids: ['face_photo', 'intimate_photo']},
			],
			reason: 'intimate_image_abuse',
			category: 'doxxing',
		},
		{
			name: 'S8 S9 age stated',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'too_young'},
				{screen_id: 'age_stated_message', option_id: 'age_yes'},
			],
			reason: 'underage',
			category: 'underage_user',
		},
		{
			name: 'S11 S12 self-harm worry',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'self_harm'},
				{screen_id: 'self_harm', option_id: 'worried_self_harm'},
			],
			reason: 'wellbeing_concern',
			category: 'self_harm',
		},
		{
			name: 'S14 harmful false claims',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'violence_misinfo'},
				{screen_id: 'violence_misinfo', option_id: 'false_info'},
				{screen_id: 'false_info', option_id: 'harmful_false_claims'},
			],
			reason: 'harmful_false_claims',
			category: 'other',
		},
		{
			name: 'S15 fraud',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'impersonation'},
				{screen_id: 'impersonation', option_id: 'fraud'},
			],
			reason: 'fraud',
			category: 'illegal_activity',
		},
		{
			name: 'S18 to S21 csam',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'abuse'},
				{screen_id: 'abuse', option_id: 'sexual'},
				{screen_id: 'sexual', option_id: 'minor_sexual'},
				{screen_id: 'minor_sexual', option_id: 'csam'},
			],
			reason: 'csam',
			category: 'child_safety',
		},
		{
			name: 'S1 to S5 terrorism',
			target: 'message',
			steps: [
				{screen_id: 'root_message', option_id: 'violence_misinfo'},
				{screen_id: 'violence_misinfo', option_id: 'terrorism'},
			],
			reason: 'terrorism_extremism',
			category: 'violent_content',
		},
		{
			name: 'P1 to P3 harassment',
			target: 'user',
			steps: [
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['photo', 'profile_text']},
				{screen_id: 'root_user', option_id: 'abuse'},
				{screen_id: 'profile_abuse', option_id: 'harassment'},
			],
			reason: 'harassment',
			category: 'harassment',
		},
		{
			name: 'P4 self-harm worry',
			target: 'user',
			steps: [
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['profile_text']},
				{screen_id: 'root_user', option_id: 'something_else'},
				{screen_id: 'something_else_user', option_id: 'self_harm'},
				{screen_id: 'crisis_support'},
				{screen_id: 'self_harm_profile', option_id: 'worried'},
			],
			reason: 'wellbeing_concern',
			category: 'other',
		},
		{
			name: 'P5 private information',
			target: 'user',
			steps: [
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['profile_text']},
				{screen_id: 'root_user', option_id: 'something_else'},
				{screen_id: 'something_else_user', option_id: 'private_info'},
				{screen_id: 'profile_private_info', item_ids: ['phone', 'address']},
			],
			reason: 'doxxing',
			category: 'inappropriate_profile',
		},
		{
			name: 'user DSA starts at the profile parts',
			target: 'user',
			surface: 'dsa',
			steps: [
				{screen_id: 'profile_parts', item_ids: ['name']},
				{screen_id: 'root_user', option_id: 'impersonation'},
				{screen_id: 'impersonation', option_id: 'impersonation_staff'},
			],
			reason: 'impersonation_staff',
			category: 'impersonation',
		},
		{
			name: 'guild DSA raid',
			target: 'guild',
			surface: 'dsa',
			steps: [
				{screen_id: 'community_parts', item_ids: ['activity']},
				{screen_id: 'root_guild', option_id: 'abuse'},
				{screen_id: 'abuse_guild', option_id: 'raid'},
			],
			reason: 'raid',
			category: 'raid_coordination',
		},
		{
			name: 'message DSA copyright notice',
			target: 'message',
			surface: 'dsa',
			steps: [
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'copyright_notice'},
			],
			reason: 'copyright',
			category: 'other',
		},
	];

	test.each(walks)('$name resolves to $reason', ({target, surface, steps, reason, category}) => {
		const resolved = resolve(target, surface ?? 'in_app', steps);
		expect(resolved.reason).toBe(reason);
		expect(resolved.legacyCategory).toBe(category);
		expect(resolved.isCurrentRevision).toBe(true);
		expect(JSON.parse(resolved.stepsJson)).toEqual(resolved.steps);
		expect(resolved.steps.map((step) => step.screen_id)).toEqual(steps.map((step) => step.screen_id));
	});

	test('checklist items are stored in definition order', () => {
		const resolved = resolve('message', 'in_app', [
			{screen_id: 'root_message', option_id: 'private_info'},
			{screen_id: 'private_info', item_ids: ['phone', 'email']},
		]);
		expect(resolved.steps[1]).toEqual({screen_id: 'private_info', item_ids: ['email', 'phone']});
		expect(resolved.stepsJson).toBe(
			'[{"screen_id":"root_message","option_id":"private_info"},{"screen_id":"private_info","item_ids":["email","phone"]}]',
		);
	});

	test('the retired violence row on the something-else screen is rejected', () => {
		for (const surface of ['in_app', 'dsa'] as const) {
			expectInvalidStep(
				'message',
				surface,
				[
					{screen_id: 'root_message', option_id: 'something_else'},
					{screen_id: 'something_else_message', option_id: 'violence_misinfo'},
					{screen_id: 'violence_misinfo', option_id: 'terrorism'},
				],
				1,
			);
		}
	});

	test('no-report endings and links are rejected', () => {
		expectInvalidStep('message', 'in_app', [{screen_id: 'root_message', option_id: 'dislike'}], 0);
		expectInvalidStep(
			'message',
			'in_app',
			[
				{screen_id: 'root_message', option_id: 'abuse'},
				{screen_id: 'abuse', option_id: 'rude_language'},
			],
			1,
		);
		expectInvalidStep(
			'message',
			'in_app',
			[
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'too_young'},
				{screen_id: 'age_stated_message', option_id: 'age_no'},
			],
			2,
		);
		expectInvalidStep(
			'user',
			'in_app',
			[
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['profile_text']},
				{screen_id: 'root_user', option_id: 'something_else'},
				{screen_id: 'something_else_user', option_id: 'too_young'},
				{screen_id: 'age_stated_profile', option_id: 'age_no'},
			],
			4,
		);
		expectInvalidStep('message', 'in_app', [{screen_id: 'root_message', option_id: 'dsa'}], 0);
		expectInvalidStep(
			'message',
			'in_app',
			[
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'copyright'},
			],
			1,
		);
		expectInvalidStep(
			'user',
			'in_app',
			[
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['photo']},
				{screen_id: 'root_user', option_id: 'something_else'},
				{screen_id: 'something_else_user', option_id: 'self_harm'},
				{screen_id: 'crisis_support', option_id: 'crisis_lines'},
			],
			4,
		);
	});

	test('in-app-only options are rejected on the DSA form, DSA-only options in the app', () => {
		expectInvalidStep(
			'message',
			'dsa',
			[
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'self_harm'},
				{screen_id: 'self_harm', option_id: 'worried_self_harm'},
			],
			2,
		);
		expectInvalidStep(
			'message',
			'in_app',
			[
				{screen_id: 'root_message', option_id: 'something_else'},
				{screen_id: 'something_else_message', option_id: 'copyright_notice'},
			],
			1,
		);
		expectInvalidStep('user', 'dsa', [{screen_id: 'profile_intro'}], 0);
	});

	test('broken walks report the first bad step', () => {
		expectInvalidStep('message', 'in_app', [{screen_id: 'abuse', option_id: 'hate'}], 0);
		expectInvalidStep('message', 'in_app', [{screen_id: 'nowhere', option_id: 'spam'}], 0);
		expectInvalidStep('message', 'in_app', [{screen_id: 'root_message', option_id: 'nope'}], 0);
		expectInvalidStep('message', 'in_app', [{screen_id: 'root_message', item_ids: ['spam']}], 0);
		expectInvalidStep(
			'message',
			'in_app',
			[
				{screen_id: 'root_message', option_id: 'abuse'},
				{screen_id: 'sexual', option_id: 'sexual_unwanted'},
			],
			1,
		);
		expectInvalidStep('message', 'in_app', [{screen_id: 'root_message', option_id: 'abuse'}], 0);
		expectInvalidStep(
			'message',
			'in_app',
			[
				{screen_id: 'root_message', option_id: 'spam'},
				{screen_id: 'abuse', option_id: 'hate'},
			],
			1,
		);
		expectInvalidStep(
			'user',
			'in_app',
			[
				{screen_id: 'profile_parts', item_ids: ['photo']},
				{screen_id: 'root_user', option_id: 'abuse'},
				{screen_id: 'profile_abuse', option_id: 'harassment'},
			],
			0,
		);
		expectInvalidStep(
			'user',
			'in_app',
			[
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['photo']},
				{screen_id: 'root_user', option_id: 'something_else'},
				{screen_id: 'something_else_user', option_id: 'self_harm'},
				{screen_id: 'self_harm_profile', option_id: 'worried'},
			],
			4,
		);
		expectInvalidStep(
			'user',
			'in_app',
			[
				{screen_id: 'profile_intro', option_id: 'learn_more'},
				{screen_id: 'profile_parts', item_ids: ['photo']},
			],
			0,
		);
		const privateInfo = {screen_id: 'root_message', option_id: 'private_info'};
		expectInvalidStep('message', 'in_app', [privateInfo, {screen_id: 'private_info', item_ids: []}], 1);
		expectInvalidStep('message', 'in_app', [privateInfo, {screen_id: 'private_info', item_ids: ['shoe_size']}], 1);
		expectInvalidStep('message', 'in_app', [privateInfo, {screen_id: 'private_info', item_ids: ['email', 'email']}], 1);
		expectInvalidStep('message', 'in_app', [privateInfo, {screen_id: 'private_info'}], 1);
		expectInvalidStep(
			'message',
			'in_app',
			[privateInfo, {screen_id: 'private_info', option_id: 'email', item_ids: ['email']}],
			1,
		);
	});

	test('the revision hash decides between a 400 and a 409', () => {
		const validSteps = [{screen_id: 'root_message', option_id: 'spam'}];
		const stale = resolveReportFlowAnswers({
			target: 'message',
			surface: 'in_app',
			revisionHash: '0000000000000000',
			steps: validSteps,
		});
		expect(stale.reason).toBe('spam');
		expect(stale.isCurrentRevision).toBe(false);
		expect(stale.currentRevisionHash).toBe(getReportFlowVariant('message', 'in_app').revisionHash);
		const outdated = captureError(() =>
			resolveReportFlowAnswers({
				target: 'message',
				surface: 'in_app',
				revisionHash: '0000000000000000',
				steps: [{screen_id: 'root_message', option_id: 'dislike'}],
			}),
		);
		expect(outdated.code).toBe(APIErrorCodes.REPORT_FLOW_OUTDATED);
		expect(outdated.status).toBe(409);
	});

	test('a flow without a variant for the surface is a validation error on surface', () => {
		const error = captureError(() => getReportFlowVariant('guild', 'in_app'));
		expect(error.code).toBe(APIErrorCodes.INVALID_FORM_BODY);
		expect(JSON.stringify(error.data)).toContain('"path":"surface"');
		expect(() => getReportFlowResponse('guild', 'in_app', 'en-US')).toThrow(FluxerError);
	});
});

describe('report reasons and legacy categories', () => {
	const legacyCategories = new Set<string>([
		...ReportMessageRequest.shape.category.options.map((option) => option.value),
		...ReportUserRequest.shape.category.options.map((option) => option.value),
		...ReportGuildRequest.shape.category.options.map((option) => option.value),
	]);

	test('there are 18 legacy categories and 56 reasons', () => {
		expect(legacyCategories.size).toBe(18);
		expect(listReportReasons()).toHaveLength(56);
		expect(findReportReason('csam')).toEqual({
			key: 'csam',
			label: 'Child sexual abuse material',
			highestPriority: true,
			legacyCategories: {message: 'child_safety', user: 'child_safety', guild: 'child_safety'},
		});
		expect(findReportReason('retired_reason')).toBeNull();
	});

	test('message and guild categories are legacy values, user categories are user values or child_safety', () => {
		for (const reason of listReportReasons()) {
			expect(legacyCategories.has(reason.legacyCategories.message), reason.key).toBe(true);
			expect(legacyCategories.has(reason.legacyCategories.guild), reason.key).toBe(true);
			const user = reason.legacyCategories.user;
			expect(user === 'child_safety' || ReportUserRequest.shape.category.safeParse(user).success, reason.key).toBe(
				true,
			);
		}
	});

	test('child reasons are child_safety for every target', () => {
		for (const key of Object.keys(REPORT_REASONS) as Array<keyof typeof REPORT_REASONS>) {
			if (key.startsWith('minor_') || key === 'csam') {
				for (const target of ['message', 'user', 'guild'] as const) {
					expect(getLegacyCategory(key, target), key).toBe('child_safety');
				}
			}
		}
	});

	test('every variant only submits catalog reasons and the user flow reaches all seven user values', () => {
		for (const [target, surface] of VARIANTS) {
			for (const reason of variantReasons(variantOf(target, surface))) {
				expect(Object.hasOwn(REPORT_REASONS, reason), reason).toBe(true);
			}
		}
		const userCategories = new Set(
			[...variantReasons(variantOf('user', 'in_app'))].map((reason) =>
				getLegacyCategory(reason as keyof typeof REPORT_REASONS, 'user'),
			),
		);
		for (const option of ReportUserRequest.shape.category.options) {
			expect(userCategories.has(option.value), option.value).toBe(true);
		}
		expect(userCategories.has('child_safety')).toBe(true);
	});
});

describe('report flow revision hash', () => {
	test('is stable, locale independent and per variant', () => {
		const first = variantOf('message', 'in_app').revisionHash;
		expect(first).toMatch(/^[0-9a-f]{16}$/);
		expect(variantOf('message', 'in_app').revisionHash).toBe(first);
		expect(getReportFlowResponse('message', 'in_app', 'en-US').revision_hash).toBe(first);
		expect(getReportFlowResponse('message', 'in_app', 'de').revision_hash).toBe(first);
		expect(getReportFlowResponse('message', 'in_app', 'ja').revision_hash).toBe(first);
		expect(variantOf('message', 'dsa').revisionHash).not.toBe(first);
		const hashes = new Set(VARIANTS.map(([target, surface]) => variantOf(target, surface).revisionHash));
		expect(hashes.size).toBe(VARIANTS.length);
	});

	test('changes when an option is added', () => {
		const added = addOption(REPORT_FLOW_LIBRARY, 'impersonation', {
			id: 'impersonation_bot',
			label: 'report_flow.label.fake_account',
			outcome: {type: 'submit', reason: 'fake_account'},
		});
		expect(variantOf('message', 'in_app', added).revisionHash).not.toBe(variantOf('message', 'in_app').revisionHash);
		expect(variantOf('guild', 'dsa', added).revisionHash).toBe(variantOf('guild', 'dsa').revisionHash);
	});
});

const EXPECTED_NOTICES: Record<ReportFlowTargetType, Array<string>> = {
	message: ['need_more_info'],
	user: ['need_more_info_profile'],
	guild: [],
};

describe('report flow rendering', () => {
	test('every variant matches the response schema and lists every reachable screen', () => {
		for (const [target, surface] of VARIANTS) {
			const response = getReportFlowResponse(target, surface, 'en-US');
			expect(ReportFlowResponse.parse(response)).toEqual(response);
			const variant = getReportFlowVariant(target, surface);
			expect(response.start_screen_id).toBe(variant.startScreenId);
			expect(response.screens.map((screen) => screen.id).sort()).toEqual([...variant.screens.keys()].sort());
			expect(response.screens[0].id).toBe(response.start_screen_id);
			expect(response.notices.map((notice) => notice.id)).toEqual(EXPECTED_NOTICES[target]);
			expect(response.locale).toBe('en-US');
		}
		expect(getReportFlowResponse('user', 'in_app', 'en-US').start_screen_id).toBe('profile_intro');
		expect(getReportFlowResponse('user', 'dsa', 'en-US').start_screen_id).toBe('profile_parts');
		expect(getReportFlowResponse('guild', 'dsa', 'en-US').start_screen_id).toBe('community_parts');
	});

	test('the rendered response is memoized per locale', () => {
		expect(getReportFlowResponse('message', 'in_app', 'en-US')).toBe(
			getReportFlowResponse('message', 'in_app', 'en-US'),
		);
		expect(getReportFlowResponse('message', 'in_app', 'de')).not.toBe(
			getReportFlowResponse('message', 'in_app', 'en-US'),
		);
	});

	test('screens render their kind, outcomes and urls', () => {
		const config = getConfig();
		const message = getReportFlowResponse('message', 'in_app', 'en-US');
		const root = message.screens.find((screen) => screen.id === 'root_message')!;
		expect(root.title).toBe('Report message');
		expect(root.options.map((option) => option.id)).toEqual([
			'abuse',
			'private_info',
			'violence_misinfo',
			'spam',
			'something_else',
			'dislike',
			'dsa',
		]);
		expect(root.options[5].outcome).toEqual({type: 'end', screen_id: null, reason: null, notice_id: null, url: null});
		expect(root.options[3].outcome).toEqual({
			type: 'submit',
			screen_id: null,
			reason: 'spam',
			notice_id: null,
			url: null,
		});
		expect(root.options[6].outcome.url).toBe(`${config.endpoints.webApp}/report`);
		const copyright = message.screens
			.find((screen) => screen.id === 'something_else_message')!
			.options.find((option) => option.id === 'copyright');
		expect(copyright?.outcome).toEqual({
			type: 'link',
			screen_id: null,
			reason: null,
			notice_id: null,
			url: `${config.endpoints.webApp}/report?option=copyright_notice`,
		});
		expect(root.checklist).toBeNull();
		expect(root.next_screen_id).toBeNull();
		const privateInfo = message.screens.find((screen) => screen.id === 'private_info')!;
		expect(privateInfo.options).toEqual([]);
		expect(privateInfo.checklist?.min_checked).toBe(1);
		expect(privateInfo.checklist?.items).toHaveLength(13);
		expect(privateInfo.checklist?.outcome.reason).toBe('doxxing');
		const ageNo = message.screens
			.find((screen) => screen.id === 'age_stated_message')!
			.options.find((option) => option.id === 'age_no');
		expect(ageNo?.outcome.notice_id).toBe('need_more_info');
		const sexual = message.screens.find((screen) => screen.id === 'sexual')!;
		expect(sexual.urgent).toBe(true);
		expect(sexual.options[0].id).toBe('minor_sexual');
		const user = getReportFlowResponse('user', 'in_app', 'en-US');
		expect(user.guidelines_url).toBe(`${config.endpoints.marketing}/guidelines`);
		const intro = user.screens[0];
		expect(intro.next_screen_id).toBe('profile_parts');
		expect(intro.options_heading).toBe('Learn more');
		expect(intro.options).toHaveLength(1);
		expect(intro.options[0].outcome.url).toBe(user.guidelines_url);
		const parts = user.screens.find((screen) => screen.id === 'profile_parts')!;
		expect(parts.checklist?.items[0]).toEqual({
			id: 'photo',
			label: 'Pictures',
			description: 'What they use as avatar and banner',
		});
		for (const surface of ['in_app', 'dsa'] as const) {
			const flow = getReportFlowResponse('user', surface, 'en-US');
			const userAgeNo = flow.screens
				.find((screen) => screen.id === 'age_stated_profile')!
				.options.find((option) => option.id === 'age_no');
			expect(userAgeNo?.outcome.notice_id).toBe('need_more_info_profile');
			const notice = flow.notices.find((entry) => entry.id === 'need_more_info_profile')!;
			expect(notice.title).toBe("We can't act on this yet");
			expect(notice.body).toMatch(/message or profile/);
		}
		const crisis = user.screens
			.find((screen) => screen.id === 'crisis_support')!
			.options.find((option) => option.id === 'crisis_lines');
		expect(crisis?.outcome.url).toBe('https://befrienders.org');
	});

	test('the DSA variants drop app-only rows and add the copyright notice', () => {
		const messageDsa = variantOf('message', 'dsa');
		expect(optionIds(messageDsa, 'root_message')).toEqual([
			'abuse',
			'private_info',
			'violence_misinfo',
			'spam',
			'something_else',
		]);
		expect(optionIds(messageDsa, 'abuse')).not.toContain('rude_language');
		expect(optionIds(messageDsa, 'self_harm')).not.toContain('worried_self_harm');
		expect(optionIds(messageDsa, 'self_harm')).not.toContain('worried_suicide');
		expect(optionIds(messageDsa, 'something_else_message')).toContain('copyright_notice');
		expect(optionIds(messageDsa, 'something_else_message')).not.toContain('copyright');
		expect(optionIds(variantOf('message', 'in_app'), 'something_else_message')).not.toContain('copyright_notice');
		const userDsa = variantOf('user', 'dsa');
		expect(userDsa.screens.has('profile_intro')).toBe(false);
		expect(optionIds(userDsa, 'root_user')).not.toContain('dsa');
		expect(optionIds(userDsa, 'self_harm_profile')).not.toContain('worried');
	});

	test('the DSA self-harm screens drop the subtitle about being worried', () => {
		const worried =
			'Nobody is punished for saying they are struggling. Telling us you are worried helps us offer support.';
		const screenOf = (target: ReportFlowTargetType, surface: ReportFlowSurface, screenId: string) =>
			getReportFlowResponse(target, surface, 'en-US').screens.find((screen) => screen.id === screenId)!;
		expect(screenOf('message', 'in_app', 'self_harm').subtitle).toBe(worried);
		expect(screenOf('user', 'in_app', 'self_harm_profile').subtitle).toBe(worried);
		expect(screenOf('message', 'dsa', 'self_harm').subtitle).toBeNull();
		expect(screenOf('user', 'dsa', 'self_harm_profile').subtitle).toBeNull();
		expect(variantOf('message', 'dsa').screens.get('self_harm')?.subtitle).toBeNull();
		expect(variantOf('user', 'dsa').screens.get('self_harm_profile')?.subtitle).toBeNull();
		expect(variantOf('message', 'in_app').screens.get('self_harm')?.subtitle).toBe(
			'report_flow.screen.self_harm.subtitle',
		);
	});

	test('the in-app copyright rows open the DSA form on the copyright notice', () => {
		const url = `${getConfig().endpoints.webApp}/report?option=copyright_notice`;
		const copyrightUrl = (target: ReportFlowTargetType, screenId: string) =>
			getReportFlowResponse(target, 'in_app', 'en-US')
				.screens.find((screen) => screen.id === screenId)
				?.options.find((option) => option.id === 'copyright')?.outcome.url;
		expect(copyrightUrl('message', 'something_else_message')).toBe(url);
		expect(copyrightUrl('user', 'something_else_user')).toBe(url);
		getConfig().instance.selfHosted = true;
		expect(copyrightUrl('message', 'something_else_message')).toBeUndefined();
		expect(copyrightUrl('user', 'something_else_user')).toBeUndefined();
	});

	test('a copyright link row that is not limited to the app throws', () => {
		const unlimited = editScreen(REPORT_FLOW_LIBRARY, 'something_else_message', (screen) => ({
			...screen,
			options: screen.options?.map((option) => (option.id === 'copyright' ? {...option, surface: undefined} : option)),
		}));
		expect(() => assertValidReportFlowLibrary(unlimited)).toThrow(/limited to in_app/);
	});

	test('a self-hosted instance drops the DSA sentence from the false information subtitle', () => {
		const dsaNames = /Digital Services Act|digitale Dienste|デジタルサービス法|الخدمات الرقمية/;
		const subtitle = (locale: string) =>
			getReportFlowResponse('message', 'in_app', locale).screens.find((screen) => screen.id === 'false_info')!
				.subtitle!;
		const hosted = Object.fromEntries(['en-US', 'de', 'ja', 'ar'].map((locale) => [locale, subtitle(locale)]));
		const hostedHash = getReportFlowResponse('message', 'in_app', 'en-US').revision_hash;
		for (const text of Object.values(hosted)) {
			expect(text).toMatch(dsaNames);
		}
		getConfig().instance.selfHosted = true;
		const selfHostedHash = getReportFlowResponse('message', 'in_app', 'en-US').revision_hash;
		expect(selfHostedHash).not.toBe(hostedHash);
		for (const locale of ['en-US', 'de', 'ja', 'ar']) {
			const text = subtitle(locale);
			expect(text, locale).not.toMatch(dsaNames);
			expect(text.length, locale).toBeGreaterThan(0);
			expect(hosted[locale].startsWith(text), locale).toBe(true);
			expect(getReportFlowResponse('message', 'in_app', locale).revision_hash, locale).toBe(selfHostedHash);
		}
		expect(subtitle('en-US')).toBe('Opinions, satire and good-faith debate are allowed.');
		expect(
			getReportFlowResponse('message', 'dsa', 'en-US').screens.find((screen) => screen.id === 'false_info')!.subtitle,
		).not.toMatch(dsaNames);
	});

	test('self-hosted instances get no guidelines, DSA or copyright link rows', () => {
		const hostedUser = getReportFlowResponse('user', 'in_app', 'en-US');
		getConfig().instance.selfHosted = true;
		const user = getReportFlowResponse('user', 'in_app', 'en-US');
		expect(user.guidelines_url).toBeNull();
		expect(user.revision_hash).not.toBe(hostedUser.revision_hash);
		const intro = user.screens[0];
		expect(intro.options).toEqual([]);
		expect(intro.options_heading).toBeNull();
		expect(intro.next_screen_id).toBe('profile_parts');
		const userScreens = new Map(user.screens.map((screen) => [screen.id, screen]));
		expect(userScreens.get('root_user')?.options.map((option) => option.id)).not.toContain('dsa');
		expect(userScreens.get('something_else_user')?.options.map((option) => option.id)).not.toContain('copyright');
		const message = getReportFlowResponse('message', 'in_app', 'en-US');
		for (const screen of message.screens) {
			for (const option of screen.options) {
				expect(option.outcome.url === null || option.outcome.url === 'https://befrienders.org', option.id).toBe(true);
			}
		}
		const messageScreens = new Map(message.screens.map((screen) => [screen.id, screen]));
		expect(messageScreens.get('root_message')?.options.map((option) => option.id)).not.toContain('dsa');
		expect(messageScreens.get('something_else_message')?.options.map((option) => option.id)).not.toContain('copyright');
		const messageDsa = getReportFlowResponse('message', 'dsa', 'en-US');
		expect(
			messageDsa.screens.find((screen) => screen.id === 'something_else_message')?.options.map((option) => option.id),
		).toContain('copyright_notice');
		resolveReportFlowAnswers({
			target: 'user',
			surface: 'in_app',
			revisionHash: user.revision_hash,
			steps: [
				{screen_id: 'profile_intro'},
				{screen_id: 'profile_parts', item_ids: ['photo']},
				{screen_id: 'root_user', option_id: 'spam'},
				{screen_id: 'spam_profile', option_id: 'spam_profile'},
			],
		});
		getConfig().instance.selfHosted = false;
		expect(getReportFlowResponse('user', 'in_app', 'en-US')).toBe(hostedUser);
	});

	test('English copy names the product, uses US spelling and never names another product', () => {
		const productName = getConfig().instance.branding.productName || 'Fluxer';
		const rendered = VARIANTS.map(([target, surface]) =>
			JSON.stringify(getReportFlowResponse(target, surface, 'en-US')),
		).join('\n');
		expect(rendered).not.toMatch(/discord/i);
		expect(rendered).not.toContain('{product_name}');
		expect(rendered).toContain(`They're under the minimum age to use ${productName}`);
		expect(rendered).toContain(`Read the ${productName} Community Guidelines`);
		expect(rendered).toContain(`${productName} staff or support`);
		expect(rendered).toContain('Sexualizing');
		expect(rendered).toContain('organization');
		expect(rendered).not.toMatch(/behaviour|organisation|colour|sexualis|labelled/);
		for (const [key, value] of Object.entries(CONTENT_I18N_MESSAGES)) {
			if (key.startsWith('report_flow.')) {
				expect(value, key).not.toMatch(/[—;]/);
				expect(value, key).not.toMatch(/discord/i);
			}
		}
	});

	test('locale tags resolve to a supported locale', () => {
		expect(resolveReportFlowLocale('nb')).toBe('no');
		expect(resolveReportFlowLocale('nb_NO')).toBe('no');
		expect(resolveReportFlowLocale('nn')).toBe('no');
		expect(resolveReportFlowLocale('zh-Hant')).toBe('zh-CN');
		expect(resolveReportFlowLocale('sv')).toBe('sv-SE');
		expect(resolveReportFlowLocale('pt')).toBe('pt-BR');
		expect(resolveReportFlowLocale('pt_BR')).toBe('pt-BR');
		expect(resolveReportFlowLocale('de')).toBe('de');
		expect(resolveReportFlowLocale('xx-YY')).toBe('en-US');
		expect(resolveReportFlowLocale(undefined)).toBe('en-US');
		expect(getReportFlowResponse('message', 'in_app', 'nb').locale).toBe('no');
		expect(getReportFlowResponse('message', 'in_app', 'xx-YY').locale).toBe('en-US');
	});
});

describe('report flow answers for staff', () => {
	test('describes a stored walk in English', () => {
		const resolved = resolve('user', 'in_app', [
			{screen_id: 'profile_intro'},
			{screen_id: 'profile_parts', item_ids: ['profile_text', 'photo']},
			{screen_id: 'root_user', option_id: 'abuse'},
			{screen_id: 'profile_abuse', option_id: 'harassment'},
		]);
		const steps = parseReportFlowSteps(resolved.stepsJson);
		expect(steps).toEqual(resolved.steps);
		expect(
			describeReportFlowAnswers({revisionHash: 'abc', surface: 'in_app', locale: 'de', steps: steps ?? []}),
		).toEqual({
			revision_hash: 'abc',
			surface: 'in_app',
			locale: 'de',
			steps: [
				{
					screen_id: 'profile_intro',
					screen_title: 'Report profile',
					option_id: null,
					option_label: null,
					items: [],
				},
				{
					screen_id: 'profile_parts',
					screen_title: 'Which parts of their profile are a problem?',
					option_id: null,
					option_label: null,
					items: [
						{id: 'photo', label: 'Pictures'},
						{id: 'profile_text', label: 'Profile text'},
					],
				},
				{
					screen_id: 'root_user',
					screen_title: "What's wrong with their profile?",
					option_id: 'abuse',
					option_label: 'Abusive or harmful content',
					items: [],
				},
				{
					screen_id: 'profile_abuse',
					screen_title: "What's harmful about their profile?",
					option_id: 'harassment',
					option_label: 'Their profile harasses or targets me or someone else',
					items: [],
				},
			],
		});
	});

	test('unknown ids are described by their raw id', () => {
		expect(
			describeReportFlowAnswers({
				revisionHash: 'abc',
				surface: 'dsa',
				locale: null,
				steps: [
					{screen_id: 'retired_screen', option_id: 'gone'},
					{screen_id: 'root_message', option_id: 'gone'},
					{screen_id: 'private_info', item_ids: ['email', 'gone']},
				],
			}).steps,
		).toEqual([
			{screen_id: 'retired_screen', screen_title: 'retired_screen', option_id: 'gone', option_label: 'gone', items: []},
			{screen_id: 'root_message', screen_title: 'Report message', option_id: 'gone', option_label: 'gone', items: []},
			{
				screen_id: 'private_info',
				screen_title: 'Which private details are involved?',
				option_id: null,
				option_label: null,
				items: [
					{id: 'email', label: 'Email address'},
					{id: 'gone', label: 'gone'},
				],
			},
		]);
	});

	test('stored steps parse totally', () => {
		expect(parseReportFlowSteps(null)).toBeNull();
		expect(parseReportFlowSteps('')).toBeNull();
		expect(parseReportFlowSteps('{')).toBeNull();
		expect(parseReportFlowSteps('{"screen_id":"a"}')).toBeNull();
		expect(parseReportFlowSteps('[{"option_id":"a"}]')).toBeNull();
		expect(parseReportFlowSteps('[{"screen_id":"a","item_ids":[1]}]')).toBeNull();
		expect(parseReportFlowSteps('[null]')).toBeNull();
		expect(parseReportFlowSteps('[{"screen_id":"a","extra":true},{"screen_id":"b","item_ids":["c"]}]')).toEqual([
			{screen_id: 'a'},
			{screen_id: 'b', item_ids: ['c']},
		]);
	});
});
