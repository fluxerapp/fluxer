// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {OperatorPassRemovalRow, OperatorPassRow} from '@app/api/database/types/OperatorPassTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import type {User} from '@app/api/models/User';
import {
	loadOperatorPassRecognition,
	type OperatorPassRecognition,
} from '@app/api/operator_pass/OperatorPassRecognition';
import type {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	isOperatorPassCountHidden,
	isOperatorPassTimestampHidden,
} from '@app/api/user/profile_badges/PremiumBadgeMasking';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {
	OPERATOR_PASS_SOURCES,
	type OperatorPassSource,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface OperatorPassHolder {
	since: Date;
	source: OperatorPassSource;
	count: number;
}

export interface OperatorPassEntitlementState {
	pass: OperatorPassRow | null;
	removal: OperatorPassRemovalRow | null;
	recognition?: OperatorPassRecognition | null;
}

function parseOperatorPassSource(value: string): OperatorPassSource {
	return (OPERATOR_PASS_SOURCES as ReadonlyArray<string>).includes(value) ? (value as OperatorPassSource) : 'admin';
}

export function isOperatorPassRemovalActive(
	removal: OperatorPassRemovalRow | null,
	now: Date = operatorPassNow(),
): boolean {
	if (removal?.removal_state !== 'removed') {
		return false;
	}
	return removal.removed_until == null || removal.removed_until.getTime() > now.getTime();
}

export function isOperatorPassPermanentlyRemoved(removal: OperatorPassRemovalRow | null): boolean {
	return removal?.removal_state === 'removed' && removal.removed_until == null;
}

export function resolveOperatorPassHolder(
	state: OperatorPassEntitlementState,
	now: Date = operatorPassNow(),
): OperatorPassHolder | null {
	if (isOperatorPassRemovalActive(state.removal, now)) {
		return null;
	}
	const {pass} = state;
	if (!pass || pass.paused_at != null || state.recognition === null) {
		return null;
	}
	return {
		since: state.recognition?.since ?? pass.granted_at,
		source: parseOperatorPassSource(pass.source),
		count: state.recognition?.count ?? Math.max(1, pass.pass_count ?? 1),
	};
}

export function isOperatorPassEntitled(state: OperatorPassEntitlementState, now?: Date): boolean {
	return resolveOperatorPassHolder(state, now) !== null;
}

export async function loadOperatorPassEntitlementState(
	repository: OperatorPassRepository,
	userId: UserID,
): Promise<OperatorPassEntitlementState> {
	const [pass, removal] = await Promise.all([repository.findOperatorPass(userId), repository.findRemoval(userId)]);
	const recognition =
		pass && pass.paused_at == null && !isOperatorPassRemovalActive(removal)
			? await loadOperatorPassRecognition(repository, userId, pass)
			: null;
	return {pass, removal, recognition};
}

export async function dispatchOperatorPassUserUpdate(
	deps: {repository: OperatorPassRepository; userRepository: UserRepository; gatewayService: IGatewayService},
	userId: UserID,
): Promise<void> {
	try {
		const [user, state] = await Promise.all([
			deps.userRepository.findUnique(userId),
			loadOperatorPassEntitlementState(deps.repository, userId),
		]);
		if (!user) return;
		const data = {
			...mapUserToPrivateResponse(user),
			...operatorPassPrivateFields(user, resolveOperatorPassHolder(state)),
		};
		await deps.gatewayService.dispatchPresence({userId, event: 'USER_UPDATE', data});
	} catch (error) {
		Logger.warn({userId, error}, 'Failed to dispatch the Operator Pass user update');
	}
}

export function operatorPassPrivateFields(
	user: User,
	holder: OperatorPassHolder | null,
): Pick<
	UserPrivateResponse,
	| 'operator_pass_since'
	| 'operator_pass_badge_hidden'
	| 'operator_pass_badge_timestamp_hidden'
	| 'operator_pass_badge_count_hidden'
> {
	return {
		operator_pass_since: holder?.since.toISOString() ?? null,
		operator_pass_badge_hidden: user.hiddenProfileBadges.has('operator_pass'),
		operator_pass_badge_timestamp_hidden: isOperatorPassTimestampHidden(user.premiumFlags),
		operator_pass_badge_count_hidden: isOperatorPassCountHidden(user.premiumFlags),
	};
}

export function profileOperatorPassSince(params: {
	user: User;
	state: OperatorPassEntitlementState;
	viewerId: UserID;
	restrictProfile: boolean;
}): Date | null {
	const {user, state, viewerId, restrictProfile} = params;
	const isOwner = viewerId === user.id;
	if (user.isBot || (!isOwner && restrictProfile)) {
		return null;
	}
	const holder = resolveOperatorPassHolder(state);
	if (
		!holder ||
		(!isOwner && (user.hiddenProfileBadges.has('operator_pass') || isOperatorPassTimestampHidden(user.premiumFlags)))
	) {
		return null;
	}
	return holder.since;
}

export class OperatorPassEntitlementService {
	constructor(readonly repository: OperatorPassRepository) {}

	async loadState(userId: UserID): Promise<OperatorPassEntitlementState> {
		return loadOperatorPassEntitlementState(this.repository, userId);
	}

	async getHolder(user: User): Promise<OperatorPassHolder | null> {
		return resolveOperatorPassHolder(await this.loadState(user.id));
	}
}
