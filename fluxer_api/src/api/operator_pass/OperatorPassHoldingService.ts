// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import '@app/api/Config';
import type {UserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassOfferRow,
	OperatorPassOfferStatus,
	OperatorPassPurchaseRow,
	OperatorPassRow,
	OperatorPassUnitEventKind,
	OperatorPassUnitRow,
	OperatorPassUnitSource,
	OperatorPassWallTakeDownCause,
} from '@app/api/database/types/OperatorPassTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {
	dispatchOperatorPassUserUpdate,
	isOperatorPassEntitled,
	isOperatorPassPermanentlyRemoved,
	isOperatorPassRemovalActive,
	loadOperatorPassEntitlementState,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	getOperatorPassTransferLockEndsAt,
	isOperatorPassOfferOpen,
	OperatorPassHoldingQueryService,
} from '@app/api/operator_pass/OperatorPassHoldingQueryService';
import type {OperatorPassLabsRoleService} from '@app/api/operator_pass/OperatorPassLabsRole';
import {OPERATOR_PASS_WITHDRAWAL_RULE_VERSION} from '@app/api/operator_pass/OperatorPassPrices';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import type {OperatorPassWallChoice} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {invalidateOperatorPassPublicWall} from '@app/api/operator_pass/OperatorPassWallPublicationService';
import type {OperatorPassWallService} from '@app/api/operator_pass/OperatorPassWallService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {RelationshipTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {OperatorPassLastPassConfirmationRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassLastPassConfirmationRequiredError';
import {OperatorPassOfferUnavailableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassOfferUnavailableError';
import {OperatorPassPassNotTransferableError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassPassNotTransferableError';
import {OperatorPassRecipientNotFriendError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRecipientNotFriendError';
import {OperatorPassRemovedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassRemovedError';
import {OperatorPassTermsNotAcceptedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassTermsNotAcceptedError';
import {OperatorPassWallNameRequiredError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassWallNameRequiredError';
import {UnknownOperatorPassOfferError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassOfferError';
import {UnknownOperatorPassUnitError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassUnitError';
import {InvalidDiscriminatorError} from '@fluxer/errors/src/domains/user/InvalidDiscriminatorError';
import {NoUsersWithFluxertagError} from '@fluxer/errors/src/domains/user/NoUsersWithFluxertagError';
import type {IEmailService, OperatorPassOfferEnd} from '@pkgs/email/src/IEmailService';
import {OPERATOR_PASS_TERMS_VERSION} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';
import {ms} from 'itty-time';

const OFFER_SCAN_PAGE_SIZE = 500;
const OFFER_RESUME_AFTER_MS = ms('10 minutes');

interface OperatorPassUnitEventDraft {
	kind: OperatorPassUnitEventKind;
	paymentId?: string | null;
	purchaseId?: string | null;
	senderId?: UserID | null;
	recipientId?: UserID | null;
	offerId?: string | null;
	country?: string | null;
	days?: number | null;
	ruleVersion?: string | null;
	reason?: string | null;
	eventId?: string;
}

export interface OperatorPassOfferParams {
	sender: User;
	passId: string;
	recipient: {id: UserID} | {username: string; discriminator: number};
	confirmLastPass: boolean;
}

interface OperatorPassHoldingDependencies {
	repository: OperatorPassRepository;
	userRepository: UserRepository;
	roleService: OperatorPassLabsRoleService;
	gatewayService: IGatewayService;
	emailService: IEmailService;
	wallService: OperatorPassWallService;
	findPurchase: (purchaseId: string) => Promise<OperatorPassPurchaseRow | null>;
}

function isOperatorPassPaymentClean(purchase: OperatorPassPurchaseRow | null): boolean {
	if (!purchase) {
		return true;
	}
	return purchase.status === 'completed' && purchase.fraud_warning_at == null && purchase.refunded_at == null;
}

async function appendUnitEvent(
	repository: OperatorPassRepository,
	passId: string,
	at: Date,
	event: OperatorPassUnitEventDraft,
): Promise<void> {
	await repository.appendUnitEvent({
		pass_id: passId,
		occurred_at: at,
		event_id: event.eventId ?? `${process.hrtime.bigint().toString().padStart(24, '0')}-${randomUUID()}`,
		kind: event.kind,
		payment_id: event.paymentId ?? null,
		purchase_id: event.purchaseId ?? null,
		sender_id: event.senderId ?? null,
		recipient_id: event.recipientId ?? null,
		offer_id: event.offerId ?? null,
		country: event.country ?? null,
		days: event.days ?? null,
		rule_version: event.ruleVersion ?? null,
		reason: event.reason ?? null,
	});
}

function isTransferUnderway(
	offer: OperatorPassOfferRow,
	unit: OperatorPassUnitRow | null,
): unit is OperatorPassUnitRow & {status: 'held'} {
	return (
		offer.status === 'accepted' &&
		unit?.status === 'held' &&
		unit.holder_id === offer.recipient_id &&
		unit.source === 'transfer'
	);
}

export async function endOperatorPassesForDeletion(
	repository: OperatorPassRepository,
	userId: UserID,
): Promise<Array<UserID>> {
	const now = operatorPassNow();
	const buyers = new Set<UserID>();
	for (const offer of await repository.listOffersByUser(userId)) {
		if (!isOperatorPassOfferOpen(offer)) {
			continue;
		}
		const unit = await repository.findUnit(offer.pass_id);
		if (isTransferUnderway(offer, unit)) {
			if (offer.recipient_id === userId && (await revokeForDeletion(repository, unit, userId, now))) {
				await repository.removeUnitFromHolder(unit.pass_id, offer.sender_id);
				if (unit.buyer_id != null && unit.buyer_id !== userId) buyers.add(unit.buyer_id);
			}
			continue;
		}
		if (await repository.updateOffer(offer, {status: Db.set('cancelled'), ended_at: Db.set(now)})) {
			await appendUnitEvent(repository, offer.pass_id, now, {
				kind: 'offer_cancelled',
				offerId: offer.offer_id,
				senderId: offer.sender_id,
				recipientId: offer.recipient_id,
				reason: 'deletion',
			});
		}
	}
	for (const unit of await repository.listUnitsByHolder(userId)) {
		if (unit.status !== 'held') {
			continue;
		}
		if (await revokeForDeletion(repository, unit, userId, now)) {
			if (unit.buyer_id != null && unit.buyer_id !== userId) buyers.add(unit.buyer_id);
		}
	}
	return [...buyers];
}

async function revokeForDeletion(
	repository: OperatorPassRepository,
	unit: OperatorPassUnitRow,
	userId: UserID,
	now: Date,
): Promise<boolean> {
	const revoked = await repository.updateUnit(unit, {
		status: Db.set('revoked'),
		ended_at: Db.set(now),
		end_reason: Db.set('deletion'),
	});
	if (!revoked) {
		return false;
	}
	await repository.removeUnitFromHolder(unit.pass_id, userId);
	await appendUnitEvent(repository, unit.pass_id, now, {
		kind: 'revoked',
		purchaseId: unit.purchase_id,
		senderId: userId,
		reason: 'deletion',
	});
	return true;
}

function operatorPassUnitIndex(passId: string): number {
	const index = Number(passId.slice(passId.lastIndexOf(':') + 1));
	return Number.isFinite(index) ? index : 0;
}

export class OperatorPassHoldingService {
	readonly queries: OperatorPassHoldingQueryService;

	constructor(private readonly deps: OperatorPassHoldingDependencies) {
		this.queries = new OperatorPassHoldingQueryService(deps);
	}

	private async appendEvent(passId: string, at: Date, event: OperatorPassUnitEventDraft): Promise<void> {
		await appendUnitEvent(this.deps.repository, passId, at, event);
	}

	async deliverPurchase(params: {
		purchase: OperatorPassPurchaseRow;
		buyer: User;
		deliveredAt: Date;
		choice: OperatorPassWallChoice | null;
	}): Promise<Array<OperatorPassUnitRow>> {
		const {purchase, buyer, deliveredAt} = params;
		const keepListing = params.choice === null && (await this.deps.wallService.hasChoice(buyer.id));
		const quantity = Math.max(1, purchase.quantity ?? 1);
		const units: Array<OperatorPassUnitRow> = [];
		for (let index = 1; index <= quantity; index++) {
			const unit: OperatorPassUnitRow = {
				pass_id: `${purchase.checkout_session_id}:${index}`,
				purchase_id: purchase.checkout_session_id,
				buyer_id: buyer.id,
				holder_id: buyer.id,
				source: 'purchase',
				status: 'held',
				transferable: true,
				delivered_at: deliveredAt,
				first_delivered_at: deliveredAt,
				paused_at: null,
				ended_at: null,
				end_reason: null,
				withdrawal_country: purchase.withdrawal_country ?? null,
				withdrawal_days: purchase.withdrawal_days ?? null,
				version: 1,
			};
			if (!(await this.deps.repository.createUnit(unit))) {
				const existing = await this.deps.repository.findUnit(unit.pass_id);
				if (existing) {
					units.push(existing);
				}
				continue;
			}
			const payment = {paymentId: purchase.payment_intent_id ?? null, purchaseId: purchase.checkout_session_id};
			await this.appendEvent(unit.pass_id, deliveredAt, {kind: 'purchased', ...payment, recipientId: buyer.id});
			if (unit.withdrawal_country) {
				await this.appendEvent(unit.pass_id, deliveredAt, {
					kind: 'country_rule',
					...payment,
					country: unit.withdrawal_country,
					days: unit.withdrawal_days,
					ruleVersion: purchase.withdrawal_rule_version ?? OPERATOR_PASS_WITHDRAWAL_RULE_VERSION,
				});
			}
			await this.appendEvent(unit.pass_id, deliveredAt, {kind: 'delivered', ...payment, recipientId: buyer.id});
			units.push(unit);
		}
		await this.deps.wallService.deliver({
			userId: buyer.id,
			purchaseId: purchase.checkout_session_id,
			choice: keepListing ? null : (params.choice ?? {name: null, optedOut: true}),
			deliveredAt,
		});
		await this.syncHolding(buyer.id, deliveredAt, purchase);
		await this.deps.roleService.ensureOperatorRole(buyer.id);
		return units;
	}

	async syncHolding(
		userId: UserID,
		at: Date = operatorPassNow(),
		purchase: OperatorPassPurchaseRow | null = null,
	): Promise<OperatorPassRow | null> {
		const held = await this.queries.listHeldUnits(userId);
		const current = await this.deps.repository.findOperatorPass(userId);
		const [first] = held;
		if (first === undefined) {
			if (current) {
				await this.deps.repository.deleteOperatorPass(userId);
				await this.deps.wallService.recordCountChange(userId, at);
				invalidateOperatorPassPublicWall();
			}
			await this.syncRoleAndPresence(userId);
			return null;
		}
		let source: OperatorPassUnitSource = 'admin';
		if (held.some((unit) => unit.source === 'purchase')) {
			source = 'purchase';
		} else if (held.some((unit) => unit.source === 'transfer')) {
			source = 'transfer';
		}
		const ownPurchaseId = first.buyer_id === userId ? (first.purchase_id ?? null) : null;
		let firstPurchase: OperatorPassPurchaseRow | null = null;
		if (ownPurchaseId && ownPurchaseId === purchase?.checkout_session_id) {
			firstPurchase = purchase;
		} else if (ownPurchaseId) {
			firstPurchase = await this.deps.findPurchase(ownPurchaseId);
		}
		const allPaused = held.every((unit) => unit.paused_at != null);
		const row: OperatorPassRow = {
			user_id: userId,
			granted_at: first.delivered_at,
			source,
			pass_count: held.length,
			checkout_session_id: ownPurchaseId,
			payment_intent_id: firstPurchase?.payment_intent_id ?? null,
			amount_minor: firstPurchase
				? Math.round(firstPurchase.amount_minor / Math.max(1, firstPurchase.quantity ?? 1))
				: null,
			currency: firstPurchase?.currency ?? null,
			paused_at: allPaused ? (current?.paused_at ?? at) : null,
			terms_version:
				current?.terms_version ?? firstPurchase?.operator_pass_terms_version ?? OPERATOR_PASS_TERMS_VERSION,
			terms_accepted_at: current?.terms_accepted_at ?? firstPurchase?.created_at ?? at,
			waiver_consent_at: current?.waiver_consent_at ?? firstPurchase?.waiver_consent_at ?? null,
			waiver_consent_version: current?.waiver_consent_version ?? firstPurchase?.waiver_consent_version ?? null,
			waiver_consent_ip: current?.waiver_consent_ip ?? firstPurchase?.waiver_consent_ip ?? null,
			waiver_consent_user_agent: current?.waiver_consent_user_agent ?? firstPurchase?.waiver_consent_user_agent ?? null,
			version: (current?.version ?? 0) + 1,
		};
		await this.deps.repository.saveOperatorPass(row);
		await this.deps.wallService.recordCountChange(userId, at);
		invalidateOperatorPassPublicWall();
		await this.syncRoleAndPresence(userId);
		return row;
	}

	async setPurchasePaused(purchase: OperatorPassPurchaseRow, paused: boolean): Promise<Array<UserID>> {
		const at = operatorPassNow();
		const holders = new Set<UserID>();
		for (const unit of await this.deps.repository.listUnitsByPurchase(purchase.checkout_session_id)) {
			if (unit.status !== 'held' || (unit.paused_at != null) === paused) {
				continue;
			}
			await this.deps.repository.updateUnit(unit, {paused_at: paused ? Db.set(at) : Db.clear()});
			await this.appendEvent(unit.pass_id, at, {
				kind: paused ? 'paused' : 'resumed',
				paymentId: purchase.payment_intent_id ?? null,
				purchaseId: purchase.checkout_session_id,
			});
			holders.add(unit.holder_id);
		}
		for (const holder of holders) {
			const before = await this.deps.repository.findOperatorPass(holder);
			const after = await this.syncHolding(holder, at);
			if (paused && after?.paused_at && !before?.paused_at) {
				await this.deps.wallService.recordHidden(holder, 'pause');
			}
			if (!paused && before?.paused_at && after && !after.paused_at) {
				await this.deps.wallService.recordShownAgain(holder);
			}
		}
		if (!holders.has(purchase.purchaser_id)) {
			await this.syncHolding(purchase.purchaser_id, at);
		}
		return [...new Set([...holders, purchase.purchaser_id])];
	}

	async endPurchaseUnits(
		purchase: OperatorPassPurchaseRow,
		cause: OperatorPassWallTakeDownCause,
		status: 'withdrawn' | 'revoked',
		limit: number = Number.POSITIVE_INFINITY,
		only: ReadonlySet<string> | null = null,
	): Promise<Array<UserID>> {
		const holders = new Set<UserID>();
		const held = (await this.deps.repository.listUnitsByPurchase(purchase.checkout_session_id))
			.filter((unit) => unit.status === 'held' && (only === null || only.has(unit.pass_id)))
			.sort((left, right) => operatorPassUnitIndex(right.pass_id) - operatorPassUnitIndex(left.pass_id))
			.slice(0, limit);
		for (const unit of held) {
			await this.endUnit(unit, status, cause, purchase.payment_intent_id ?? null);
			holders.add(unit.holder_id);
		}
		for (const holder of holders) {
			await this.afterHolderLostUnits(holder, cause, purchase.checkout_session_id);
			if (holder !== purchase.purchaser_id && status === 'revoked') {
				await this.notifyRevoked(holder, cause);
			}
		}
		if (!holders.has(purchase.purchaser_id)) {
			await this.syncHolding(purchase.purchaser_id);
		}
		return [...new Set([...holders, purchase.purchaser_id])];
	}

	async withdrawUnit(unit: OperatorPassUnitRow, purchase: OperatorPassPurchaseRow): Promise<void> {
		await this.endUnit(unit, 'withdrawn', 'withdrawal', purchase.payment_intent_id ?? null);
		await this.afterHolderLostUnits(unit.holder_id, 'withdrawal', purchase.checkout_session_id);
		if (unit.buyer_id != null && unit.buyer_id !== unit.holder_id) {
			await this.syncHolding(unit.buyer_id);
		}
	}

	async createOffer(params: OperatorPassOfferParams): Promise<OperatorPassOfferRow> {
		const {sender, passId} = params;
		const unit = await this.deps.repository.findUnit(passId);
		if (!unit || unit.holder_id !== sender.id || unit.status !== 'held') {
			throw new UnknownOperatorPassUnitError();
		}
		if (!unit.transferable || unit.source !== 'purchase') {
			throw new OperatorPassPassNotTransferableError();
		}
		const senderState = await loadOperatorPassEntitlementState(this.deps.repository, sender.id);
		if (isOperatorPassRemovalActive(senderState.removal)) {
			throw new OperatorPassRemovedError();
		}
		const senderOffers = await this.deps.repository.listOffersByUser(sender.id);
		if (senderOffers.some((offer) => offer.pass_id === passId && isOperatorPassOfferOpen(offer))) {
			throw new OperatorPassOfferUnavailableError();
		}
		const recipientId = await this.resolveRecipient(params.recipient);
		const recipient = recipientId === sender.id ? null : await this.deps.userRepository.findUnique(recipientId);
		const recipientState = recipient
			? await loadOperatorPassEntitlementState(this.deps.repository, recipient.id)
			: null;
		if (
			!recipient ||
			!recipientState ||
			recipient.isBot ||
			(recipient.flags & UserFlags.DELETED) !== 0n ||
			recipient.pendingDeletionAt ||
			isOperatorPassPermanentlyRemoved(recipientState.removal)
		) {
			throw new OperatorPassOfferUnavailableError();
		}
		if (!(await this.areFriends(sender.id, recipient.id))) {
			throw new OperatorPassRecipientNotFriendError();
		}
		const held = await this.queries.listHeldUnits(sender.id);
		const offered = new Set(
			senderOffers
				.filter((offer) => offer.sender_id === sender.id && isOperatorPassOfferOpen(offer))
				.map((offer) => offer.pass_id),
		);
		const keeping = held.filter((row) => !offered.has(row.pass_id)).length;
		if (keeping <= 1 && !params.confirmLastPass) {
			throw new OperatorPassLastPassConfirmationRequiredError();
		}
		const now = operatorPassNow();
		const purchase = unit.purchase_id ? await this.deps.findPurchase(unit.purchase_id) : null;
		if (!isOperatorPassPaymentClean(purchase)) {
			throw new OperatorPassPassNotTransferableError();
		}
		const offer: OperatorPassOfferRow = {
			offer_id: randomUUID(),
			pass_id: passId,
			sender_id: sender.id,
			recipient_id: recipientId,
			status: 'pending',
			created_at: now,
			accepted_at: null,
			completes_at: getOperatorPassTransferLockEndsAt(purchase, unit),
			ended_at: null,
			wall_name: null,
			wall_opt_out: null,
			terms_version: null,
			version: 1,
		};
		await this.deps.repository.createOffer(offer);
		await this.appendEvent(passId, now, {
			kind: 'offer_created',
			offerId: offer.offer_id,
			senderId: sender.id,
			recipientId,
			purchaseId: unit.purchase_id,
			paymentId: purchase?.payment_intent_id ?? null,
		});
		await this.dispatchPassesUpdate([sender.id, recipientId]);
		if (recipient.email) {
			await this.deps.emailService.send(
				'operator_pass_offer_received',
				recipient.email,
				{
					senderName: sender.globalName ?? sender.username,
					arrivesAt: offer.completes_at,
					timeZone: recipient.timezone,
				},
				recipient.locale,
			);
		}
		return offer;
	}

	private async areFriends(a: UserID, b: UserID): Promise<boolean> {
		const [forward, reverse] = await Promise.all([
			this.deps.userRepository.getRelationship(a, b, RelationshipTypes.FRIEND),
			this.deps.userRepository.getRelationship(b, a, RelationshipTypes.FRIEND),
		]);
		return forward !== null && reverse !== null;
	}

	private async resolveRecipient(recipient: {id: UserID} | {username: string; discriminator: number}): Promise<UserID> {
		if ('id' in recipient) {
			return recipient.id;
		}
		if (!Number.isInteger(recipient.discriminator) || recipient.discriminator < 0 || recipient.discriminator > 9999) {
			throw new InvalidDiscriminatorError();
		}
		const user = recipient.username
			? await this.deps.userRepository.findByUsernameDiscriminator(recipient.username, recipient.discriminator)
			: null;
		if (!user) {
			throw new NoUsersWithFluxertagError();
		}
		return user.id;
	}

	async withdrawOffer(sender: User, offerId: string): Promise<OperatorPassOfferRow> {
		const offer = await this.queries.requireOffer(offerId);
		if (offer.sender_id !== sender.id || !isOperatorPassOfferOpen(offer)) {
			throw new UnknownOperatorPassOfferError();
		}
		if (isTransferUnderway(offer, await this.deps.repository.findUnit(offer.pass_id))) {
			throw new OperatorPassOfferUnavailableError();
		}
		return this.endOffer(offer, 'withdrawn', 'offer_withdrawn', offer.recipient_id, 'withdrawn');
	}

	async declineOffer(recipient: User, offerId: string): Promise<OperatorPassOfferRow> {
		const offer = await this.queries.requireOffer(offerId);
		if (offer.recipient_id !== recipient.id || offer.status !== 'pending') {
			throw new UnknownOperatorPassOfferError();
		}
		return this.endOffer(offer, 'declined', 'offer_declined', offer.sender_id, 'declined');
	}

	async acceptOffer(params: {
		recipient: User;
		offerId: string;
		acceptedTerms: boolean;
		choice: OperatorPassWallChoice | null;
	}): Promise<OperatorPassOfferRow> {
		const {recipient, offerId} = params;
		const offer = await this.queries.requireOffer(offerId);
		if (offer.recipient_id !== recipient.id || offer.status !== 'pending') {
			throw new UnknownOperatorPassOfferError();
		}
		if (!params.acceptedTerms) {
			throw new OperatorPassTermsNotAcceptedError();
		}
		const choice = params.choice ?? ((await this.deps.wallService.hasChoice(recipient.id)) ? null : undefined);
		if (choice === undefined) {
			throw new OperatorPassWallNameRequiredError();
		}
		const state = await loadOperatorPassEntitlementState(this.deps.repository, recipient.id);
		if (isOperatorPassPermanentlyRemoved(state.removal)) {
			throw new OperatorPassRemovedError();
		}
		if (!(await this.areFriends(offer.sender_id, recipient.id))) {
			throw new OperatorPassRecipientNotFriendError();
		}
		const now = operatorPassNow();
		const accepted = await this.deps.repository.updateOffer(offer, {
			status: Db.set('accepted'),
			accepted_at: Db.set(now),
			wall_name: Db.set(choice && !choice.optedOut ? choice.name : null),
			wall_opt_out: Db.set(choice ? choice.optedOut : null),
			terms_version: Db.set(OPERATOR_PASS_TERMS_VERSION),
		});
		if (!accepted) {
			throw new OperatorPassOfferUnavailableError();
		}
		await this.appendEvent(offer.pass_id, now, {
			kind: 'offer_accepted',
			offerId,
			senderId: offer.sender_id,
			recipientId: recipient.id,
		});
		const result = (await this.completeOfferIfDue(accepted)) ?? accepted;
		await this.dispatchPassesUpdate([offer.sender_id, recipient.id]);
		return result;
	}

	async completeDueOffers(): Promise<number> {
		let completed = 0;
		let pageState: string | null = null;
		do {
			const page = await this.deps.repository.scanOffers(OFFER_SCAN_PAGE_SIZE, pageState);
			for (const offer of page.rows) {
				if (offer.status !== 'accepted') {
					continue;
				}
				try {
					if ((await this.completeOfferIfDue(offer))?.status === 'completed') {
						completed += 1;
					}
				} catch (error) {
					Logger.error({error, offerId: offer.offer_id}, 'Failed to complete an Operator Pass offer');
				}
			}
			pageState = page.pageState;
		} while (pageState);
		return completed;
	}

	private async cancelOffersForPass(passId: string, reason: OperatorPassOfferEnd): Promise<void> {
		const unit = await this.deps.repository.findUnit(passId);
		if (!unit) {
			return;
		}
		const offers = (await this.deps.repository.listOffersByUser(unit.holder_id)).filter(
			(offer) => offer.pass_id === passId && isOperatorPassOfferOpen(offer),
		);
		for (const offer of offers) {
			if (isTransferUnderway(offer, unit)) {
				const purchase = unit.purchase_id ? await this.deps.findPurchase(unit.purchase_id) : null;
				await this.finishTransfer(offer, unit, purchase, unit.delivered_at);
				continue;
			}
			await this.endOffer(offer, 'cancelled', 'offer_cancelled', offer.recipient_id, reason);
		}
	}

	async cancelOffersBetween(a: UserID, b: UserID): Promise<void> {
		const offers = (await this.deps.repository.listOffersByUser(a)).filter(
			(offer) => offer.status === 'pending' && (offer.sender_id === b || offer.recipient_id === b),
		);
		for (const offer of offers) {
			const ended = await this.deps.repository.updateOffer(offer, {
				status: Db.set('cancelled'),
				ended_at: Db.set(operatorPassNow()),
			});
			if (ended) {
				await this.announceOfferEnded(ended, 'offer_cancelled', offer.recipient_id, 'cancelled');
			}
		}
	}

	private async completeOfferIfDue(offer: OperatorPassOfferRow): Promise<OperatorPassOfferRow | null> {
		const now = operatorPassNow();
		const unit = await this.deps.repository.findUnit(offer.pass_id);
		const purchase = unit?.purchase_id ? await this.deps.findPurchase(unit.purchase_id) : null;
		if (isTransferUnderway(offer, unit)) {
			if (now.getTime() - unit.delivered_at.getTime() < OFFER_RESUME_AFTER_MS) {
				return offer;
			}
			return this.finishTransfer(offer, unit, purchase, unit.delivered_at);
		}
		if (unit?.status !== 'held' || unit.holder_id !== offer.sender_id) {
			if (offer.status === 'accepted') {
				await this.afterHolderLostUnits(offer.sender_id, 'passed_on', unit?.purchase_id ?? null);
			}
			return this.endOffer(offer, 'cancelled', 'offer_cancelled', offer.recipient_id, 'cancelled');
		}
		if (
			purchase &&
			(purchase.fraud_warning_at != null || purchase.refunded_at != null || purchase.status !== 'completed')
		) {
			if (purchase.status === 'disputed') {
				return offer;
			}
			return this.endOffer(offer, 'cancelled', 'offer_cancelled', offer.recipient_id, 'cancelled');
		}
		const lockEndsAt = getOperatorPassTransferLockEndsAt(purchase, unit);
		if (lockEndsAt.getTime() > now.getTime()) {
			return offer;
		}
		const moved = await this.deps.repository.updateUnit(unit, {
			holder_id: Db.set(offer.recipient_id),
			source: Db.set('transfer'),
			transferable: Db.set(false),
			delivered_at: Db.set(now),
		});
		if (!moved) {
			const latest = await this.deps.repository.findUnit(unit.pass_id);
			const stillOpen = latest?.status === 'held' && [offer.sender_id, offer.recipient_id].includes(latest.holder_id);
			return stillOpen ? null : this.endOffer(offer, 'cancelled', 'offer_cancelled', offer.recipient_id, 'cancelled');
		}
		return this.finishTransfer(offer, moved, purchase, now);
	}

	private async finishTransfer(
		offer: OperatorPassOfferRow,
		unit: OperatorPassUnitRow,
		purchase: OperatorPassPurchaseRow | null,
		movedAt: Date,
	): Promise<OperatorPassOfferRow | null> {
		await this.deps.repository.moveUnitHolder(unit.pass_id, offer.sender_id, offer.recipient_id);
		const payment = {paymentId: purchase?.payment_intent_id ?? null, purchaseId: unit.purchase_id};
		await this.appendEvent(unit.pass_id, movedAt, {
			kind: 'transferred',
			...payment,
			offerId: offer.offer_id,
			senderId: offer.sender_id,
			recipientId: offer.recipient_id,
			eventId: `${offer.offer_id}-1-transferred`,
		});
		await this.appendEvent(unit.pass_id, movedAt, {
			kind: 'delivered',
			...payment,
			recipientId: offer.recipient_id,
			eventId: `${offer.offer_id}-2-delivered`,
		});
		await this.afterHolderLostUnits(offer.sender_id, 'passed_on', unit.purchase_id);
		const wallService = this.deps.wallService;
		const delivery: Parameters<OperatorPassWallService['deliver']>[0] = {
			userId: offer.recipient_id,
			purchaseId: unit.purchase_id,
			choice: null,
			deliveredAt: movedAt,
		};
		if (offer.wall_opt_out != null) {
			delivery.choice = {
				name: offer.wall_opt_out ? null : (offer.wall_name ?? null),
				optedOut: offer.wall_opt_out !== false,
			};
		}
		await wallService.deliver(delivery);
		await this.syncHolding(offer.recipient_id, movedAt);
		await this.deps.roleService.ensureOperatorRole(offer.recipient_id);
		const completed = await this.deps.repository.updateOffer(offer, {
			status: Db.set('completed'),
			ended_at: Db.set(movedAt),
		});
		if (!completed) {
			return null;
		}
		const [sender, recipient] = await Promise.all([
			this.deps.userRepository.findUnique(offer.sender_id),
			this.deps.userRepository.findUnique(offer.recipient_id),
		]);
		if (sender?.email) {
			await this.deps.emailService.send(
				'operator_pass_offer_accepted',
				sender.email,
				{
					recipientName: recipient ? (recipient.globalName ?? recipient.username) : '',
					remainingCount: (await this.queries.listHeldUnits(offer.sender_id)).length,
				},
				sender.locale,
			);
		}
		return completed;
	}

	private async endOffer(
		offer: OperatorPassOfferRow,
		status: OperatorPassOfferStatus,
		kind: OperatorPassUnitEventKind,
		notifyUserId: UserID,
		reason: OperatorPassOfferEnd,
	): Promise<OperatorPassOfferRow> {
		const ended = await this.deps.repository.updateOffer(offer, {
			status: Db.set(status),
			ended_at: Db.set(operatorPassNow()),
		});
		if (!ended) {
			throw new OperatorPassOfferUnavailableError();
		}
		await this.announceOfferEnded(ended, kind, notifyUserId, reason);
		return ended;
	}

	private async announceOfferEnded(
		offer: OperatorPassOfferRow,
		kind: OperatorPassUnitEventKind,
		notifyUserId: UserID,
		reason: OperatorPassOfferEnd,
	): Promise<void> {
		await this.appendEvent(offer.pass_id, offer.ended_at ?? operatorPassNow(), {
			kind,
			offerId: offer.offer_id,
			senderId: offer.sender_id,
			recipientId: offer.recipient_id,
		});
		await this.dispatchPassesUpdate([offer.sender_id, offer.recipient_id]);
		const [notified, other] = await Promise.all([
			this.deps.userRepository.findUnique(notifyUserId),
			this.deps.userRepository.findUnique(notifyUserId === offer.sender_id ? offer.recipient_id : offer.sender_id),
		]);
		if (notified?.email) {
			await this.deps.emailService.send(
				'operator_pass_offer_ended',
				notified.email,
				{otherName: other ? (other.globalName ?? other.username) : '', ended: reason},
				notified.locale,
			);
		}
	}

	private async endUnit(
		unit: OperatorPassUnitRow,
		status: 'withdrawn' | 'revoked',
		reason: string,
		paymentId: string | null,
	): Promise<void> {
		const now = operatorPassNow();
		await this.cancelOffersForPass(unit.pass_id, 'cancelled');
		const ended = await this.deps.repository.updateUnit(unit, {
			status: Db.set(status),
			ended_at: Db.set(now),
			end_reason: Db.set(reason),
		});
		if (!ended) {
			throw new Error('Operator Pass changed while it was being ended');
		}
		await this.deps.repository.removeUnitFromHolder(unit.pass_id, unit.holder_id);
		await this.appendEvent(unit.pass_id, now, {
			kind: status,
			paymentId,
			purchaseId: unit.purchase_id,
			senderId: unit.holder_id,
			reason,
		});
	}

	private async afterHolderLostUnits(
		holderId: UserID,
		cause: OperatorPassWallTakeDownCause,
		purchaseId: string | null,
	): Promise<void> {
		const row = await this.syncHolding(holderId);
		if (!row) {
			await this.deps.wallService.takeDown(holderId, cause, purchaseId);
		}
	}

	private async notifyRevoked(holderId: UserID, cause: OperatorPassWallTakeDownCause): Promise<void> {
		const holder = await this.deps.userRepository.findUnique(holderId);
		if (!holder?.email) {
			return;
		}
		await this.deps.emailService.send(
			'operator_pass_revoked',
			holder.email,
			{
				cause: cause === 'dispute' || cause === 'withdrawal' ? cause : 'refund',
				lastPass: (await this.queries.listHeldUnits(holderId)).length === 0,
			},
			holder.locale,
		);
	}

	private async syncRoleAndPresence(userId: UserID): Promise<void> {
		const user = await this.deps.userRepository.findUnique(userId);
		const entitled =
			user !== null && isOperatorPassEntitled(await loadOperatorPassEntitlementState(this.deps.repository, userId));
		await this.deps.roleService.syncOperatorRole(userId, entitled);
		await this.dispatchPassesUpdate([userId]);
		await dispatchOperatorPassUserUpdate(
			{
				repository: this.deps.repository,
				userRepository: this.deps.userRepository,
				gatewayService: this.deps.gatewayService,
			},
			userId,
		);
	}

	async dispatchPassesUpdate(userIds: ReadonlyArray<UserID>): Promise<void> {
		for (const userId of new Set(userIds)) {
			try {
				const user = await this.deps.userRepository.findUnique(userId);
				if (!user) {
					continue;
				}
				await this.deps.gatewayService.dispatchPresence({
					userId,
					event: 'OPERATOR_PASS_UPDATE',
					data: await this.queries.describePasses(user),
				});
			} catch (error) {
				Logger.warn({userId, error}, 'Failed to dispatch the Operator Pass update');
			}
		}
	}
}
