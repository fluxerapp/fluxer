// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import type {UserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {
	OperatorPassWallEntryRow,
	OperatorPassWallRevisionRow,
	OperatorPassWallRevisionSource,
	OperatorPassWallTakeDownCause,
} from '@app/api/database/types/OperatorPassTypes';
import {getOperatorPassWallInfrastructure} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {validateOperatorPassBusinessLogo} from '@app/api/operator_pass/OperatorPassBusinessListing';
import {
	loadOperatorPassEntitlementState,
	resolveOperatorPassHolder,
} from '@app/api/operator_pass/OperatorPassEntitlementService';
import {OperatorPassReleaseService} from '@app/api/operator_pass/OperatorPassReleaseService';
import type {OperatorPassRepository, OperatorPassWallEntryPatch} from '@app/api/operator_pass/OperatorPassRepository';
import {operatorPassNow} from '@app/api/operator_pass/OperatorPassSchedule';
import {
	liveRevisionPatch,
	OperatorPassWallEntryStore,
	type WallEventDraft,
} from '@app/api/operator_pass/OperatorPassWallEntryStore';
import {OperatorPassWallHistoryService} from '@app/api/operator_pass/OperatorPassWallHistoryService';
import {
	type OperatorPassWallDecisionResult,
	OperatorPassWallModerationService,
} from '@app/api/operator_pass/OperatorPassWallModerationService';
import {
	hasOperatorPassWallChoice,
	isOperatorPassWallListed,
	type OperatorPassWallChoice,
} from '@app/api/operator_pass/OperatorPassWallPresentation';
import {
	invalidateOperatorPassPublicWall,
	OperatorPassWallPublicationService,
} from '@app/api/operator_pass/OperatorPassWallPublicationService';
import {OperatorPassWallRetentionService} from '@app/api/operator_pass/OperatorPassWallRetentionService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';

export class OperatorPassWallService {
	private readonly entries: OperatorPassWallEntryStore;
	readonly releases: OperatorPassReleaseService;
	readonly publication: OperatorPassWallPublicationService;
	readonly retention: OperatorPassWallRetentionService;
	readonly history: OperatorPassWallHistoryService;
	readonly moderation: OperatorPassWallModerationService;

	constructor(
		private readonly repository: OperatorPassRepository,
		private readonly userRepository: UserRepository,
	) {
		this.entries = new OperatorPassWallEntryStore(repository, invalidateOperatorPassPublicWall);
		this.releases = new OperatorPassReleaseService(repository, userRepository, this.entries);
		this.publication = new OperatorPassWallPublicationService(repository, userRepository, this.entries);
		this.retention = new OperatorPassWallRetentionService(repository, this.entries, this.publication, this.auditLogs);
		this.history = new OperatorPassWallHistoryService(repository, userRepository, this.entries);
		this.moderation = new OperatorPassWallModerationService(repository, userRepository, this.entries, this.publication);
	}

	private readonly auditLogs = new AdminRepository();

	async getEntry(userId: UserID): Promise<OperatorPassWallEntryRow | null> {
		return this.repository.findWallEntry(userId);
	}

	async hasChoice(userId: UserID): Promise<boolean> {
		return hasOperatorPassWallChoice(await this.repository.findWallEntry(userId));
	}

	async deliver(params: {
		userId: UserID;
		purchaseId: string | null;
		choice: OperatorPassWallChoice | null;
		deliveredAt: Date;
	}): Promise<void> {
		let before: OperatorPassWallEntryRow | null = null;
		const entry = await this.entries.mutate(params.userId, params.deliveredAt, params.userId, (current) => {
			before = current;
			const events: Array<WallEventDraft> = [{kind: 'delivered', purchaseId: params.purchaseId}];
			const patch: OperatorPassWallEntryPatch = {
				delivered_at: Db.set(current.delivered_at ?? params.deliveredAt),
				updated_at: Db.set(params.deliveredAt),
			};
			if (!params.choice) {
				if (!current.opted_out && current.listed_at == null) {
					patch.listed_at = Db.set(params.deliveredAt);
					patch.left_wall_at = Db.set(null);
					events.push({kind: 'listed', name: current.name});
				}
				return {patch, events};
			}
			if (params.choice.optedOut) {
				patch.opted_out = Db.set(true);
				patch.listed_at = Db.set(null);
				patch.left_wall_at = Db.set(current.left_wall_at ?? params.deliveredAt);
				events.push({kind: 'opted_out'});
				if (isOperatorPassWallListed(current)) {
					events.push({kind: 'taken_down', name: current.live_name, cause: 'holder_request'});
				}
			} else {
				patch.opted_out = Db.set(false);
				patch.listed_at = Db.set(current.listed_at ?? params.deliveredAt);
				patch.left_wall_at = Db.set(null);
				events.push({kind: 'listed', name: params.choice.name});
			}
			return {patch, events};
		});
		if (params.choice && !params.choice.optedOut && params.choice.name) {
			await this.submitRevision(params.userId, params.choice.name, 'checkout', params.userId, {useProfileAvatar: true});
		}
		await this.publication.publishChanges([params.userId]);
		await this.syncListedAvatar(before, entry);
	}

	async setChoice(
		userId: UserID,
		choice: OperatorPassWallChoice,
		actorId: UserID = userId,
		{keepAsIs, releaseListOptIn}: {keepAsIs?: boolean; releaseListOptIn?: boolean} = {},
	): Promise<void> {
		const now = operatorPassNow();
		let before: OperatorPassWallEntryRow | null = null;
		const entry = await this.entries.mutate(userId, now, actorId, (current) => {
			before = current;
			const patch: OperatorPassWallEntryPatch = {updated_at: Db.set(now)};
			const events: Array<WallEventDraft> = [];
			if (keepAsIs !== undefined && keepAsIs !== current.keep_as_is) {
				patch.keep_as_is = Db.set(keepAsIs);
			}
			if (releaseListOptIn !== undefined && releaseListOptIn !== (current.release_list_opt_in ?? false)) {
				patch.release_list_opt_in = Db.set(releaseListOptIn);
			}
			if (choice.optedOut) {
				if (!current.opted_out) {
					patch.opted_out = Db.set(true);
					patch.listed_at = Db.set(null);
					patch.left_wall_at = Db.set(now);
					events.push({kind: 'opted_out'});
					if (isOperatorPassWallListed(current)) {
						events.push({kind: 'taken_down', name: current.live_name, cause: 'holder_request'});
					}
				}
			} else if (!isOperatorPassWallListed(current)) {
				patch.opted_out = Db.set(false);
				patch.listed_at = Db.set(now);
				patch.left_wall_at = Db.set(null);
				events.push({kind: 'listed', name: choice.name});
			}
			return Object.keys(patch).length > 1 || events.length > 0 ? {patch, events} : null;
		});
		if (!choice.optedOut && choice.name) {
			const latest = entry.pending_name ?? entry.live_name;
			if (latest !== choice.name) {
				await this.submitRevision(userId, choice.name, 'settings', actorId, {useProfileAvatar: true});
			}
		}
		await this.publication.publishChanges([userId]);
		await this.syncListedAvatar(before, entry);
	}

	private async syncListedAvatar(
		before: OperatorPassWallEntryRow | null,
		after: OperatorPassWallEntryRow,
	): Promise<void> {
		const wasListed = before != null && isOperatorPassWallListed(before);
		const isListed = isOperatorPassWallListed(after);
		if (wasListed && !isListed && before.live_avatar) {
			await this.publication.releaseAvatar(before.live_avatar);
		} else if (!wasListed && isListed) {
			await this.publication.publishAvatarIfServed(after);
		}
	}

	async onProfileChanged(
		user: User,
		previous: {username: string; globalName: string | null; avatarHash: string | null},
	): Promise<void> {
		const entry = await this.repository.findWallEntry(user.id);
		if (!entry || !isOperatorPassWallListed(entry)) {
			return;
		}
		const avatarChanged = (previous.avatarHash ?? null) !== (user.avatarHash ?? null);
		const previousDisplay = previous.globalName ?? previous.username;
		const currentDisplay = user.globalName ?? user.username;
		const latestName = entry.pending_name ?? entry.live_name ?? entry.name ?? currentDisplay;
		const followsName = latestName === previousDisplay && previousDisplay !== currentDisplay;
		if (entry.live_revision_id == null && previous.username !== user.username) {
			await this.publication.publishChanges([user.id], 'preserve');
		}
		if (entry.keep_as_is || entry.live_business || (!avatarChanged && !followsName)) {
			return;
		}
		await this.submitRevision(user.id, followsName ? currentDisplay : latestName, 'profile', user.id, {
			useProfileAvatar: true,
			user,
		});
	}

	async submitRevision(
		userId: UserID,
		name: string,
		source: OperatorPassWallRevisionSource,
		actorId: UserID | null,
		options: {useProfileAvatar?: boolean; avatar?: string | null; user?: User; business?: boolean} = {},
	): Promise<OperatorPassWallRevisionRow> {
		const now = operatorPassNow();
		let avatar = options.avatar ?? null;
		if (options.useProfileAvatar) {
			const user = options.user ?? (await this.userRepository.findUnique(userId));
			avatar = user ? await getOperatorPassWallInfrastructure().avatarStore.snapshotUserAvatar(user) : null;
		}
		const revision: OperatorPassWallRevisionRow = {
			user_id: userId,
			revision_id: await getOperatorPassWallInfrastructure().nextId(),
			name,
			avatar,
			business: options.business ?? false,
			source,
			created_at: now,
			created_by: actorId,
		};
		await this.repository.appendWallRevision(revision);
		if (avatar) {
			await this.repository.addWallAvatarRef(avatar, userId);
		}
		await this.entries.mutate(userId, now, actorId, () => ({
			patch: {
				name: Db.set(name),
				pending_revision_id: Db.set(revision.revision_id),
				pending_name: Db.set(name),
				pending_avatar: Db.set(avatar),
				pending_source: Db.set(source),
				pending_since: Db.set(now),
				updated_at: Db.set(now),
			},
			events: source === 'checkout' ? [] : [{kind: 'changed', name}],
		}));
		return revision;
	}

	async setBusinessListing(
		userId: UserID,
		adminId: UserID,
		input: {name: string; logo: Uint8Array},
	): Promise<OperatorPassWallDecisionResult> {
		await validateOperatorPassBusinessLogo(input.logo);
		const avatar = await getOperatorPassWallInfrastructure().avatarStore.storePrivate(input.logo);
		await this.submitRevision(userId, input.name, 'admin', adminId, {avatar, business: true});
		await this.setChoice(userId, {name: input.name, optedOut: false}, adminId);
		return this.moderation.approve(userId, adminId);
	}

	async endBusinessListing(userId: UserID): Promise<void> {
		const entry = await this.repository.findWallEntry(userId);
		if (!entry?.live_business) {
			return;
		}
		const personal = await this.moderation.lastPersonalRevision(userId);
		const now = operatorPassNow();
		let ended = false;
		const next = await this.entries.mutate(userId, now, null, (current) => {
			if (!current.live_business || current.live_revision_id !== entry.live_revision_id) {
				return null;
			}
			ended = true;
			return {
				patch: {...liveRevisionPatch(personal), updated_at: Db.set(now)},
				events: [{kind: 'fallback', name: personal?.name ?? null}],
			};
		});
		if (!ended) {
			return;
		}
		await this.moderation.recordBusinessEnded(userId, entry.live_revision_id ?? null, personal, null, now);
		await this.publication.publishAvatarIfServed(next);
		await this.publication.publishChanges([userId], 'preserve');
		if (entry.live_avatar && entry.live_avatar !== personal?.avatar) {
			await this.publication.releaseAvatar(entry.live_avatar);
		}
	}

	async takeDown(userId: UserID, cause: OperatorPassWallTakeDownCause, purchaseId: string | null): Promise<void> {
		const now = operatorPassNow();
		const existing = await this.repository.findWallEntry(userId);
		const personal = existing?.live_business ? await this.moderation.lastPersonalRevision(userId) : null;
		let endedBusiness: {revisionId: bigint | null; avatar: string | null} | null = null;
		const entry = await this.entries.mutate(
			userId,
			now,
			null,
			(current) => {
				if (current.delivered_at == null && current.listed_at == null) {
					return null;
				}
				const events: Array<WallEventDraft> = isOperatorPassWallListed(current)
					? [{kind: 'taken_down', name: current.live_name, cause, purchaseId}]
					: [];
				const business = current.live_business === true && current.live_revision_id === existing?.live_revision_id;
				endedBusiness = business
					? {revisionId: current.live_revision_id ?? null, avatar: current.live_avatar ?? null}
					: null;
				return {
					patch: {
						...(business ? liveRevisionPatch(personal) : {}),
						listed_at: Db.set(null),
						delivered_at: Db.set(null),
						left_wall_at: Db.set(current.left_wall_at ?? now),
						updated_at: Db.set(now),
					},
					events,
				};
			},
			'create',
		);
		const business = endedBusiness as {revisionId: bigint | null; avatar: string | null} | null;
		if (business) {
			await this.moderation.recordBusinessEnded(userId, business.revisionId, personal, null, now);
		}
		await this.publication.publishChanges([userId]);
		for (const avatar of new Set([entry.live_avatar, business?.avatar])) {
			if (avatar) {
				await this.publication.releaseAvatar(avatar);
			}
		}
	}

	async forgetForDeletion(userId: UserID): Promise<void> {
		const now = operatorPassNow();
		const existing = await this.repository.findWallEntry(userId);
		if (!existing) {
			return;
		}
		await this.entries.appendEvent(userId, now, {kind: 'taken_down', name: null, cause: 'deletion'}, null);
		const entry = await this.entries.mutate(userId, now, null, (current) => {
			if (current.name == null && current.listed_at == null && current.delivered_at == null) {
				return null;
			}
			return {
				patch: {
					name: Db.set(null),
					opted_out: Db.set(true),
					listed_at: Db.set(null),
					left_wall_at: Db.set(current.left_wall_at ?? now),
					delivered_at: Db.set(null),
					updated_at: Db.set(now),
				},
				events: [],
			};
		});
		await this.publication.publishChanges([userId]);
		if (entry.live_avatar) {
			await this.publication.releaseAvatar(entry.live_avatar);
		}
	}

	async recordHidden(userId: UserID, cause: OperatorPassWallTakeDownCause): Promise<void> {
		const entry = await this.repository.findWallEntry(userId);
		if (entry && isOperatorPassWallListed(entry)) {
			await this.entries.appendEvent(
				userId,
				operatorPassNow(),
				{kind: 'taken_down', name: entry.live_name, cause},
				null,
			);
		}
		await this.publication.publishChanges([userId]);
		if (entry?.live_avatar) {
			await this.publication.releaseAvatar(entry.live_avatar);
		}
	}

	async recordCountChange(userId: UserID, at: Date = operatorPassNow()): Promise<void> {
		const holder = resolveOperatorPassHolder(await loadOperatorPassEntitlementState(this.repository, userId), at);
		const passCount = holder?.count ?? 0;
		const firstPassAt = holder?.since ?? null;
		const previous = (await this.repository.listWallEvents(userId)).findLast((event) => event.kind === 'count_changed');
		if (previous?.pass_count !== passCount || previous?.first_pass_at?.getTime() !== firstPassAt?.getTime()) {
			await this.entries.appendEvent(userId, at, {kind: 'count_changed', passCount, firstPassAt}, null);
		}
		await this.publication.publishChanges([userId]);
		const entry = passCount > 0 ? await this.repository.findWallEntry(userId) : null;
		if (entry) {
			await this.publication.publishAvatarIfServed(entry);
		}
	}

	async recordShownAgain(userId: UserID): Promise<void> {
		const entry = await this.repository.findWallEntry(userId);
		if (entry && isOperatorPassWallListed(entry)) {
			await this.entries.appendEvent(userId, operatorPassNow(), {kind: 'shown_again', name: entry.live_name}, null);
			await this.publication.publishAvatarIfServed(entry);
		}
		await this.publication.publishChanges([userId]);
	}
}
