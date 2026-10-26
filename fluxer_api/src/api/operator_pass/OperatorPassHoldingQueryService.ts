// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {
	OperatorPassOfferRow,
	OperatorPassOfferStatus,
	OperatorPassPurchaseRow,
	OperatorPassUnitRow,
} from '@app/api/database/types/OperatorPassTypes';
import type {User} from '@app/api/models/User';
import {
	isOperatorPassRemovalActive,
	loadOperatorPassEntitlementState,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {
	getOperatorPassWithdrawalDeadline,
	getOperatorPassWithdrawalEndsOn,
	isOperatorPassUnitWithdrawable,
} from '@app/api/operator_pass/OperatorPassPurchaseRules';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {UnknownOperatorPassOfferError} from '@fluxer/errors/src/domains/operator_pass/UnknownOperatorPassOfferError';
import type {
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
	OperatorPassUnitResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

interface OperatorPassHoldingQueryDependencies {
	repository: OperatorPassRepository;
	userRepository: UserRepository;
	findPurchase: (purchaseId: string) => Promise<OperatorPassPurchaseRow | null>;
}

const OPEN_OFFER_STATUSES: ReadonlyArray<OperatorPassOfferStatus> = ['pending', 'accepted'];

export function isOperatorPassOfferOpen(offer: OperatorPassOfferRow): boolean {
	return OPEN_OFFER_STATUSES.includes(offer.status);
}

export function getOperatorPassTransferLockEndsAt(
	purchase: OperatorPassPurchaseRow | null,
	unit: OperatorPassUnitRow,
): Date {
	const paidAt = purchase?.purchased_at ?? unit.first_delivered_at;
	const ruleMs = (unit.withdrawal_days ?? 0) * MS_PER_DAY;
	const lockMs = paidAt.getTime() + Math.max(Config.operatorPass.transferLockMs, ruleMs);
	const deadline = unit.withdrawal_days != null && purchase ? getOperatorPassWithdrawalDeadline(purchase) : null;
	return new Date(Math.max(lockMs, deadline?.getTime() ?? 0));
}

function mapOperatorPassOfferUser(user: User | undefined): OperatorPassOfferResponse['sender'] {
	if (!user) {
		return null;
	}
	return {
		id: user.id.toString(),
		username: user.username,
		global_name: user.globalName ?? null,
		avatar: user.avatarHash ?? null,
	};
}

function mapOperatorPassOffer(
	offer: OperatorPassOfferRow,
	users: ReadonlyMap<UserID, User>,
): OperatorPassOfferResponse {
	return {
		id: offer.offer_id,
		pass_id: offer.pass_id,
		status: offer.status,
		sender: mapOperatorPassOfferUser(users.get(offer.sender_id)),
		recipient: mapOperatorPassOfferUser(users.get(offer.recipient_id)),
		created_at: offer.created_at.toISOString(),
		accepted_at: offer.accepted_at?.toISOString() ?? null,
		arrives_at: (offer.completes_at ?? offer.created_at).toISOString(),
		ended_at: offer.ended_at?.toISOString() ?? null,
	};
}

export class OperatorPassHoldingQueryService {
	constructor(private readonly deps: OperatorPassHoldingQueryDependencies) {}

	async listHeldUnits(userId: UserID): Promise<Array<OperatorPassUnitRow>> {
		return (await this.deps.repository.listUnitsByHolder(userId)).filter((unit) => unit.status === 'held');
	}

	async requireOffer(offerId: string): Promise<OperatorPassOfferRow> {
		const offer = await this.deps.repository.findOffer(offerId);
		if (!offer) {
			throw new UnknownOperatorPassOfferError();
		}
		return offer;
	}

	async describePasses(user: User): Promise<OperatorPassPassesResponse> {
		const now = operatorPassNow();
		const [units, offers, state] = await Promise.all([
			this.listHeldUnits(user.id),
			this.deps.repository.listOffersByUser(user.id),
			loadOperatorPassEntitlementState(this.deps.repository, user.id),
		]);
		const open = offers.filter(isOperatorPassOfferOpen);
		const completed = offers
			.filter((offer) => offer.status === 'completed')
			.sort((left, right) => (left.ended_at?.getTime() ?? 0) - (right.ended_at?.getTime() ?? 0));
		const users = await this.loadOfferUsers([...open, ...completed]);
		const purchases = new Map<string, OperatorPassPurchaseRow | null>();
		for (const purchaseId of new Set(units.map((unit) => unit.purchase_id).filter((id) => id !== null))) {
			purchases.set(purchaseId, await this.deps.findPurchase(purchaseId));
		}
		units.sort((left, right) => left.delivered_at.getTime() - right.delivered_at.getTime());
		const passes: Array<OperatorPassUnitResponse> = units.map((unit) => {
			const purchase = unit.purchase_id ? (purchases.get(unit.purchase_id) ?? null) : null;
			const canPassOn = unit.transferable && unit.source === 'purchase';
			const withdrawable = purchase !== null && isOperatorPassUnitWithdrawable(purchase, unit, now);
			const offer = open.find((candidate) => candidate.pass_id === unit.pass_id && candidate.sender_id === user.id);
			const received = completed.findLast(
				(candidate) => candidate.pass_id === unit.pass_id && candidate.recipient_id === user.id,
			);
			return {
				id: unit.pass_id,
				source: unit.source,
				status: unit.status,
				purchase_id: unit.purchase_id,
				delivered_at: unit.delivered_at.toISOString(),
				paused: unit.paused_at != null,
				can_pass_on: canPassOn,
				transfer_lock_ends_at: canPassOn ? getOperatorPassTransferLockEndsAt(purchase, unit).toISOString() : null,
				can_withdraw: withdrawable,
				withdrawal_ends_at:
					purchase?.withdrawal_days != null && unit.source === 'purchase'
						? (getOperatorPassWithdrawalDeadline(purchase)?.toISOString() ?? null)
						: null,
				withdrawal_ends_on:
					purchase?.withdrawal_days != null && purchase.withdrawal_ends_at && unit.source === 'purchase'
						? getOperatorPassWithdrawalEndsOn(purchase.withdrawal_ends_at)
						: null,
				offer: offer ? mapOperatorPassOffer(offer, users) : null,
				received_from: received ? mapOperatorPassOfferUser(users.get(received.sender_id)) : null,
			};
		});
		return {
			count: isOperatorPassRemovalActive(state.removal, now) ? 0 : units.length,
			recognition_count: resolveOperatorPassHolder(state, now)?.count ?? 0,
			passes,
			incoming_offers: open
				.filter((offer) => offer.recipient_id === user.id && offer.sender_id !== user.id)
				.map((offer) => mapOperatorPassOffer(offer, users)),
			outgoing_offers: open
				.filter((offer) => offer.sender_id === user.id)
				.map((offer) => mapOperatorPassOffer(offer, users)),
			passed_on: completed
				.filter((offer) => offer.sender_id === user.id && offer.recipient_id !== user.id)
				.map((offer) => mapOperatorPassOffer(offer, users)),
		};
	}

	async describeOffer(user: User, offerId: string): Promise<OperatorPassOfferResponse> {
		const offer = await this.requireOffer(offerId);
		if (offer.sender_id !== user.id && offer.recipient_id !== user.id) {
			throw new UnknownOperatorPassOfferError();
		}
		return mapOperatorPassOffer(offer, await this.loadOfferUsers([offer]));
	}

	private async loadOfferUsers(offers: ReadonlyArray<OperatorPassOfferRow>): Promise<Map<UserID, User>> {
		const ids = new Set<UserID>();
		for (const offer of offers) {
			ids.add(offer.sender_id);
			ids.add(offer.recipient_id);
		}
		const users = new Map<UserID, User>();
		for (const id of ids) {
			const found = await this.deps.userRepository.findUnique(id);
			if (found) {
				users.set(id, found);
			}
		}
		return users;
	}
}
