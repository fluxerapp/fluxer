// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import type {UserContactChangeLogRow} from '@app/api/database/types/UserTypes';
import type {User} from '@app/api/models/User';
import type {UserContactChangeLogRepository} from '@app/api/user/repositories/UserContactChangeLogRepository';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';

type ContactChangeReason = 'user_requested' | 'admin_action';

interface ContactChange {
	field: 'email' | 'has_verified_phone' | 'fluxer_tag';
	oldValue: string | null;
	newValue: string | null;
}

interface RecordDiffParams {
	oldUser: User | null;
	newUser: User;
	reason: ContactChangeReason;
	actorUserId: UserID | null;
	eventAt?: Date;
}

interface ListLogsParams {
	userId: UserID;
	limit?: number;
	beforeEventId?: string;
}

export class UserContactChangeLogService {
	private readonly DEFAULT_LIMIT = 50;

	constructor(private readonly repo: UserContactChangeLogRepository) {}

	async recordDiff(params: RecordDiffParams): Promise<void> {
		const {oldUser, newUser, reason, actorUserId, eventAt} = params;
		const changes: Array<ContactChange> = [
			{
				field: 'email',
				oldValue: oldUser?.email?.toLowerCase() ?? null,
				newValue: newUser.email?.toLowerCase() ?? null,
			},
			{
				field: 'has_verified_phone',
				oldValue: String(oldUser?.hasVerifiedPhone ?? false),
				newValue: String(newUser.hasVerifiedPhone),
			},
			{
				field: 'fluxer_tag',
				oldValue: this.buildFluxerTag(oldUser),
				newValue: this.buildFluxerTag(newUser),
			},
		];
		await awaitAll(
			changes
				.filter(({oldValue, newValue}) => oldValue !== newValue)
				.map(async (change) =>
					this.repo.insertLog({
						userId: newUser.id,
						...change,
						reason,
						actorUserId,
						eventAt,
					}),
				),
			'Failed to record user contact changes',
		);
	}

	async listLogs(params: ListLogsParams): Promise<Array<UserContactChangeLogRow>> {
		const {userId, beforeEventId} = params;
		const limit = params.limit ?? this.DEFAULT_LIMIT;
		return this.repo.listLogs({userId, limit, beforeEventId});
	}

	private buildFluxerTag(user: User | null): string | null {
		if (!user) return null;
		const discriminator = user.discriminator?.toString() ?? '';
		if (!user.username || discriminator === '') {
			return null;
		}
		const paddedDiscriminator = discriminator.padStart(4, '0');
		return `${user.username}#${paddedDiscriminator}`;
	}
}
