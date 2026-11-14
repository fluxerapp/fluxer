// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, listTestEmails, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import type {
	OperatorPassPurchaseRow,
	OperatorPassRow,
	OperatorPassUnitRow,
} from '@app/api/database/types/OperatorPassTypes';
import {getOperatorPassWallService} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	OperatorPassListingResponse,
	OperatorPassWallResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {UserProfileFullResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {expect, test} from 'vitest';

async function givePass(holderId: UserID): Promise<void> {
	const repository = new OperatorPassRepository();
	const deliveredAt = new Date('2026-11-01T12:00:00Z');
	await repository.saveOperatorPass({
		user_id: holderId,
		granted_at: deliveredAt,
		source: 'purchase',
		pass_count: 1,
		paused_at: null,
		version: 1,
	} as OperatorPassRow);
	await repository.createPurchase({
		checkout_session_id: `quote-${holderId}`,
		purchaser_id: holderId,
		quantity: 1,
		version: 1,
	} as OperatorPassPurchaseRow);
	await repository.createUnit({
		pass_id: `quote-${holderId}:1`,
		purchase_id: `quote-${holderId}`,
		buyer_id: holderId,
		holder_id: holderId,
		source: 'purchase',
		status: 'held',
		transferable: true,
		delivered_at: deliveredAt,
		first_delivered_at: deliveredAt,
		paused_at: null,
		ended_at: null,
		end_reason: null,
		withdrawal_country: null,
		withdrawal_days: null,
		version: 1,
	} as OperatorPassUnitRow);
}

test('a quote shows on the wall and the badge only while the listing is shown', async () => {
	const harness = await createApiTestHarness();
	try {
		const holder = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		const holderId = createUserID(BigInt(holder.userId));
		await givePass(holderId);

		await createBuilder(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_name: 'Ada', wall_opt_out: false, wall_quote: 'see example.com'})
			.expect(400, APIErrorCodes.OPERATOR_PASS_WALL_QUOTE_REJECTED)
			.execute();
		await createBuilder(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_name: 'Ada', wall_opt_out: false, wall_quote: 'bad\u202Equote'})
			.expect(400, APIErrorCodes.OPERATOR_PASS_WALL_QUOTE_INVALID)
			.execute();
		const listing = await createBuilder<OperatorPassListingResponse>(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_name: 'Ada', wall_opt_out: false, wall_quote: '  <b>Chat for everyone</b> '})
			.expect(200)
			.execute();
		expect(listing.wall_quote).toBe('<b>Chat for everyone</b>');

		const shownQuotes = async () => {
			const wall = await createBuilder<OperatorPassWallResponse>(harness, viewer.token)
				.get('/operator-pass/wall')
				.expect(200)
				.execute();
			const profile = await createBuilder<UserProfileFullResponse>(harness, holder.token)
				.get(`/users/${holder.userId}/profile`)
				.expect(200)
				.execute();
			return {
				wall: wall.operators.find((operator) => operator.name === 'Ada')?.quote ?? null,
				badge: profile.profile_badges?.find((badge) => badge.kind === 'operator_pass')?.quote ?? null,
			};
		};
		expect(await shownQuotes()).toEqual({wall: '<b>Chat for everyone</b>', badge: '<b>Chat for everyone</b>'});

		const moderation = getOperatorPassWallService().moderation;
		await moderation.setHidden(holderId, holderId, true, {reason: 'guidelines', note: 'test'});
		expect(await shownQuotes()).toEqual({wall: null, badge: null});
		await moderation.setHidden(holderId, holderId, false, {reason: null, note: null});
		expect(await shownQuotes()).toEqual({wall: '<b>Chat for everyone</b>', badge: '<b>Chat for everyone</b>'});

		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_TEMP_BAN]);
		const clearQuote = (quote: string) =>
			createBuilder(harness, admin.token)
				.post(`/admin/users/${holder.userId}/operator-pass/wall/clear-quote`)
				.header('X-Audit-Log-Reason', 'quote test')
				.body({reason: 'impersonation', note: 'The quote poses as a staff announcement.', quote});
		await clearTestEmails(harness);
		await clearQuote('an older quote').expect(400).execute();
		expect(await shownQuotes()).toEqual({wall: '<b>Chat for everyone</b>', badge: '<b>Chat for everyone</b>'});
		await clearQuote('<b>Chat for everyone</b>').expect(200).execute();
		expect(await shownQuotes()).toEqual({wall: null, badge: null});
		const statements = (await listTestEmails(harness, {recipient: holder.email})).filter(
			(email) => email.type === 'operator_pass_wall_quote_cleared',
		);
		expect(statements.map((email) => email.metadata)).toEqual([
			{reason: 'impersonation', note: 'The quote poses as a staff announcement.'},
		]);
		const decisions = await new OperatorPassRepository().listWallDecisions(holderId);
		expect(decisions.find((decision) => decision.kind === 'clear_quote')).toMatchObject({
			reason: 'impersonation',
			statement_id: expect.any(String),
		});

		await createBuilder(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_name: 'Ada', wall_opt_out: false, wall_quote: 'Back again'})
			.expect(200)
			.execute();
		await createBuilder(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_opt_out: true})
			.expect(200)
			.execute();
		await createBuilder(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_name: 'Ada', wall_opt_out: false})
			.expect(200)
			.execute();
		expect(await shownQuotes()).toEqual({wall: null, badge: null});
	} finally {
		await harness.shutdown();
	}
});

test('a quote may mention Fluxer and its team but not links', async () => {
	const harness = await createApiTestHarness();
	try {
		const holder = await createTestAccount(harness);
		await givePass(createUserID(BigInt(holder.userId)));
		const setQuote = (quote: string) =>
			createBuilder<OperatorPassListingResponse>(harness, holder.token)
				.put('/operator-pass/listing')
				.body({wall_name: 'Ada', wall_opt_out: false, wall_quote: quote});

		for (const quote of ['Proud to support Fluxer', 'Fluxer team forever']) {
			const listing = await setQuote(quote).expect(200).execute();
			expect(listing.wall_quote).toBe(quote);
		}
		for (const quote of ['Join me at https://fluxer.app', 'Mail me at ada@example.com']) {
			await setQuote(quote).expect(400, APIErrorCodes.OPERATOR_PASS_WALL_QUOTE_REJECTED).execute();
		}
	} finally {
		await harness.shutdown();
	}
});

test('hiding the pass count removes it from a frozen wall', async () => {
	const harness = await createApiTestHarness();
	try {
		const holder = await createTestAccount(harness);
		const viewer = await createTestAccount(harness);
		const holderId = createUserID(BigInt(holder.userId));
		await givePass(holderId);
		await createBuilder(harness, holder.token)
			.put('/operator-pass/listing')
			.body({wall_name: 'Ada', wall_opt_out: false, wall_quote: 'Chat for everyone'})
			.expect(200)
			.execute();

		const shownOperator = async () => {
			const wall = await createBuilder<OperatorPassWallResponse>(harness, viewer.token)
				.get('/operator-pass/wall')
				.expect(200)
				.execute();
			return wall.operators.find((operator) => operator.name === 'Ada');
		};
		expect(await shownOperator()).toMatchObject({count: 1, quote: 'Chat for everyone'});

		await getOperatorPassWallService().publication.setFrozen(true, holderId);
		await createBuilder(harness, holder.token)
			.patch('/users/@me')
			.body({operator_pass_badge_count_hidden: true})
			.expect(200)
			.execute();
		const operator = await shownOperator();
		expect(operator).toMatchObject({name: 'Ada', quote: 'Chat for everyone'});
		expect(operator).not.toHaveProperty('count');
	} finally {
		await harness.shutdown();
	}
});
