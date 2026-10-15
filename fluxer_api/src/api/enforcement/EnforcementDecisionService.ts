// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BannedIpEntry} from '@app/api/admin/AdminRepository';
import type {AdminAuditService} from '@app/api/admin/services/AdminAuditService';
import type {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import {queueMessageShredJob} from '@app/api/admin/services/AdminMessageShredService';
import type {AdminReportService} from '@app/api/admin/services/AdminReportService';
import {createChannelID, createMessageID, createReportID, createUserID, type UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {
	EnforcementDecisionNoteRow,
	EnforcementDecisionRow,
	EvidenceChannelKind,
} from '@app/api/database/types/EnforcementTypes';
import type {IARMessageContextRow} from '@app/api/database/types/ReportTypes';
import {type ClosureStateDeps, cancelClosure} from '@app/api/enforcement/ClosureState';
import {loadDecisionNotice} from '@app/api/enforcement/EnforcementAccountNotice';
import type {EnforcementActor} from '@app/api/enforcement/EnforcementActor';
import type {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {
	toAdminDecisionResponse,
	toDecisionSummary,
	toNoteResponse,
} from '@app/api/enforcement/EnforcementDecisionResponses';
import {
	collectEvidence,
	type EvidenceSubject,
	type MessageReference,
	resolveEvidenceLocations,
	restrictedEvidence,
	snapshotEvidenceChannelKinds,
} from '@app/api/enforcement/EnforcementEvidence';
import type {EnforcementNoticeDelivery} from '@app/api/enforcement/EnforcementNoticeDelivery';
import {
	findHoldingFeatureLimit,
	listActiveFeatureLimits,
	setFeatureLimit,
} from '@app/api/enforcement/FeatureLimitState';
import {
	latestNetworkBlockExpiry,
	listNetworkBlockCandidates,
	type NetworkBlockPlan,
	planNetworkBlocks,
	type SkippedNetwork,
} from '@app/api/enforcement/NetworkBlocks';
import {hasHolderDeletionPending, liftSuspension} from '@app/api/enforcement/SuspensionState';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {emitAdminAction} from '@app/api/infrastructure/activity/AccountChangeEvents';
import {Logger} from '@app/api/Logger';
import {requireGrantedAdminACLs} from '@app/api/middleware/AdminMiddleware';
import type {Guild} from '@app/api/models/Guild';
import type {User} from '@app/api/models/User';
import {type IARSubmission, ReportStatus} from '@app/api/report/ReportModels';
import type {ReportService} from '@app/api/report/ReportService';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	ENFORCEMENT_REASONS,
	ENFORCEMENT_RULES,
	type EnforcementKind,
	type EnforcementLimit,
	type EnforcementReasonCode,
	isAccountKind,
	isEnforcementKindAllowed,
	isEnforcementLimitAllowed,
} from '@fluxer/constants/src/EnforcementReasons';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {EnforcementDecisionEndedError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionEndedError';
import {EnforcementDecisionReplacedError} from '@fluxer/errors/src/domains/moderation/EnforcementDecisionReplacedError';
import {EnforcementDeletionStartedError} from '@fluxer/errors/src/domains/moderation/EnforcementDeletionStartedError';
import {UnknownEnforcementDecisionError} from '@fluxer/errors/src/domains/moderation/UnknownEnforcementDecisionError';
import {UnknownReportError} from '@fluxer/errors/src/domains/moderation/UnknownReportError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ReportItemOutcome} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {UserProfileField} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {
	EnforcementDecisionEndRequest,
	EnforcementDecisionInput,
	EnforcementDecisionListQuery,
	EnforcementDecisionListResponse,
	EnforcementDecisionResponse,
	NetworkBlockCandidatesResponse,
	NetworkBlockRequest,
} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';

type EnforcementSubject = {type: 'user'; user: User} | {type: 'guild'; guild: Guild};

type ContentRemoval = {type: 'messages'; entries: ReadonlyArray<MessageReference>} | {type: 'all_messages'};

interface ProfileReset {
	fields: ReadonlyArray<UserProfileField>;
	values: ReadonlyMap<UserProfileField, string>;
}

interface RecordDecisionParams {
	subject: EnforcementSubject;
	kind: EnforcementKind;
	input: EnforcementDecisionInput;
	endsAt?: Date | null;
	deletionAt?: Date;
	limit?: EnforcementLimit;
	removeEvidence?: boolean;
	removal?: ContentRemoval;
	profileReset?: ProfileReset;
	blockNetworks?: NetworkBlockRequest;
	actor: EnforcementActor;
}

export interface RecordedDecision {
	decision: EnforcementDecisionRow;
	blockedNetworks: Array<BannedIpEntry>;
	skippedNetworks: Array<SkippedNetwork>;
}

interface AutomatedPurchaseLimitParams {
	subject: User;
	reasonCode: EnforcementReasonCode;
	paymentAt: Date;
	internalNote: string;
}

interface EnforcementDecisionServiceDeps extends ClosureStateDeps {
	repository: EnforcementDecisionRepository;
	delivery: EnforcementNoticeDelivery;
	auditService: AdminAuditService;
	banManagementService: AdminBanManagementService;
	channelRepository: ChannelRepository;
	guildRepository: GuildRepository;
	reportService: ReportService;
	adminReportService: AdminReportService;
}

const EVIDENCE_LIMIT = 50;
const UNDOABLE_KINDS: ReadonlySet<EnforcementKind> = new Set(['suspension', 'closure', 'feature_limit']);
const REVIEWING_KINDS: ReadonlySet<EnforcementKind> = new Set(['warning', 'suspension', 'closure']);
const CONFIRMATIONS = ['evidence_preserved', 'reports_filed_or_not_required'] as const;
const RESTRICTING_KINDS: ReadonlySet<EnforcementKind> = new Set(['suspension', 'closure', 'feature_limit']);

function kindAcls(decision: EnforcementDecisionRow): Array<string> {
	switch (decision.kind) {
		case 'suspension':
			return [AdminACLs.USER_TEMP_BAN];
		case 'closure':
			return [AdminACLs.USER_DELETE];
		case 'warning':
		case 'feature_limit':
			return [AdminACLs.USER_UPDATE_FLAGS];
		case 'content_removal':
			return [decision.removed_all_messages ? AdminACLs.MESSAGE_DELETE_ALL : AdminACLs.MESSAGE_SHRED];
		case 'profile_reset':
			return [AdminACLs.USER_UPDATE_PROFILE];
		case 'community_removal':
			return [AdminACLs.GUILD_DELETE];
	}
}

function firstNoteBody(
	input: EnforcementDecisionInput,
	internalNote: string,
	cancelledHolderDeletionAt: Date | null,
): string {
	const lines = [internalNote];
	if (input.confirmations?.length) {
		lines.push(`Confirmed by staff: ${input.confirmations.join(', ')}`);
	}
	if (cancelledHolderDeletionAt) {
		lines.push(
			`Cancelled the deletion the account holder had scheduled for ${cancelledHolderDeletionAt.toISOString()}`,
		);
	}
	return lines.join('\n\n');
}

export function decisionAuditMetadata(decision: EnforcementDecisionRow): Array<[string, string]> {
	return [
		['decision_id', decision.decision_id.toString()],
		['reason_code', decision.reason_code.toString()],
		['kind', decision.kind],
		['notice_status', decision.notice_status],
	];
}

export function networkBlockAuditMetadata({blockedNetworks}: RecordedDecision): Array<[string, string]> {
	const until = latestNetworkBlockExpiry(blockedNetworks);
	if (!until) return [];
	return [
		['blocked_ips', blockedNetworks.map((entry) => entry.ip).join(',')],
		['network_blocks_until', until.toISOString()],
	];
}

type DecisionSubjectFields = Pick<
	EnforcementDecisionRow,
	'subject_type' | 'subject_id' | 'subject_name' | 'recipient_user_id'
>;

type DecisionFields = Pick<
	EnforcementDecisionRow,
	| 'kind'
	| 'reason_code'
	| 'source'
	| 'automation'
	| 'statement'
	| 'rights_holder'
	| 'ends_at'
	| 'deletion_at'
	| 'feature_limit'
	| 'profile_fields'
	| 'removed_message_count'
	| 'removed_all_messages'
	| 'payment_at'
	| 'report_ids'
	| 'decided_by'
>;

function evidenceSubject(subject: EnforcementSubject): EvidenceSubject {
	if (subject.type === 'guild') return {type: 'guild', id: subject.guild.id};
	const {id, username, discriminator} = subject.user;
	return {type: 'user', id, username, discriminator};
}

function canBlockNetworks(kind: EnforcementKind, endsAt: Date | null | undefined): boolean {
	return kind === 'closure' || (kind === 'suspension' && !endsAt);
}

function reportOutcome(
	report: IARSubmission,
	{kind, removal, removeEvidence}: RecordDecisionParams,
	evidence: ReadonlyArray<IARMessageContextRow>,
): ReportItemOutcome | null {
	if (kind === 'community_removal') return 'removed';
	if (kind === 'profile_reset' && report.reportedMessageId === null) return 'removed';
	const messageId = report.reportedMessageId;
	if (messageId !== null) {
		const removed =
			removal?.type === 'all_messages' ||
			(removal?.type === 'messages' && removal.entries.some((entry) => entry.message_id === messageId)) ||
			(removeEvidence && evidence.some((message) => message.message_id === messageId));
		if (removed) return 'removed';
	}
	if (kind === 'warning') return 'warned';
	return RESTRICTING_KINDS.has(kind) ? 'restricted' : null;
}

export class EnforcementDecisionService {
	constructor(private readonly deps: EnforcementDecisionServiceDeps) {}

	async record(
		params: RecordDecisionParams,
		applyState: (decisionId: bigint) => Promise<void>,
	): Promise<RecordedDecision> {
		const {reports, evidence, removedMessageCount, internalNote, networkPlan} = await this.validate(params);
		const {subject, kind, input, removal, profileReset, actor} = params;
		const replacedDecisionId =
			subject.type === 'user' && isAccountKind(kind) ? subject.user.enforcementDecisionId : null;
		const restricted = ENFORCEMENT_REASONS[input.reason_code].handling === 'restricted';
		const stored = restricted ? evidence.map(restrictedEvidence) : evidence;
		const channelKinds = await snapshotEvidenceChannelKinds(evidence, this.deps.channelRepository);
		const decision = await this.persist(
			await this.subjectFields(subject),
			{
				kind,
				reason_code: input.reason_code,
				source: input.source,
				automation: input.automation,
				statement: input.statement ?? null,
				rights_holder: input.rights_holder ?? null,
				ends_at: params.endsAt ?? null,
				deletion_at: params.deletionAt ?? null,
				feature_limit: params.limit ?? null,
				profile_fields: profileReset ? new Set(profileReset.fields) : null,
				removed_message_count: removedMessageCount,
				removed_all_messages: removal?.type === 'all_messages' ? true : null,
				payment_at: null,
				report_ids: reports.length > 0 ? new Set(reports.map((report) => report.reportId)) : null,
				decided_by: actor.adminUserId,
			},
			{messages: stored, channelKinds, profileValues: profileReset?.values ?? null},
			firstNoteBody(
				input,
				internalNote,
				kind === 'suspension' && subject.type === 'user' && hasHolderDeletionPending(subject.user)
					? subject.user.pendingDeletionAt
					: null,
			),
			applyState,
		);
		if (subject.type === 'user' && REVIEWING_KINDS.has(kind)) {
			await this.clearReviewQueueFor(subject.user.id);
		}
		if (params.removeEvidence) {
			await this.queueEvidenceRemoval(decision, evidence, actor.adminUserId);
		}
		const carried = replacedDecisionId
			? await this.takeOverNetworkBlocks(replacedDecisionId, decision, canBlockNetworks(kind, params.endsAt))
			: [];
		const networks = networkPlan
			? await this.blockNetworks(decision, networkPlan)
			: {blocked: [], skipped: [] as Array<SkippedNetwork>};
		const blocked = [
			...carried.filter((entry) => !networks.blocked.some((block) => block.ip === entry.ip)),
			...networks.blocked,
		];
		await this.resolveReports(
			reports.map((report) => ({report, outcome: reportOutcome(report, params, evidence)})),
			actor,
			internalNote,
		);
		return {
			decision: await this.deliverNotice(decision, stored, channelKinds, latestNetworkBlockExpiry(blocked)),
			blockedNetworks: blocked,
			skippedNetworks: networks.skipped,
		};
	}

	async recordAutomatedPurchaseLimit({
		subject,
		reasonCode,
		paymentAt,
		internalNote,
	}: AutomatedPurchaseLimitParams): Promise<EnforcementDecisionRow> {
		const decision = await this.persist(
			await this.subjectFields({type: 'user', user: subject}),
			{
				kind: 'feature_limit',
				reason_code: reasonCode,
				source: 'own_review',
				automation: 'automated',
				statement: null,
				rights_holder: null,
				ends_at: null,
				deletion_at: null,
				feature_limit: 'purchases',
				profile_fields: null,
				removed_message_count: null,
				removed_all_messages: null,
				payment_at: paymentAt,
				report_ids: null,
				decided_by: SYSTEM_USER_ID,
			},
			{messages: [], channelKinds: new Map(), profileValues: null},
			internalNote,
			async () => {
				await setFeatureLimit(this.deps, subject, 'purchases', true, SYSTEM_USER_ID);
			},
		);
		return this.deliverNotice(decision, [], new Map(), null);
	}

	async end(
		decisionId: bigint,
		request: EnforcementDecisionEndRequest,
		actor: EnforcementActor,
	): Promise<EnforcementDecisionResponse> {
		const decision = await this.requireDecision(decisionId);
		requireGrantedAdminACLs(actor.acls, kindAcls(decision));
		if (isAccountKind(decision.kind)) {
			const networkBlocks = await this.deps.banManagementService.listDecisionNetworkBlocks(decisionId);
			if (networkBlocks.length > 0) {
				requireGrantedAdminACLs(actor.acls, [AdminACLs.BAN_IP_REMOVE]);
			}
		}
		await this.finish(decision, request, actor.adminUserId, actor.internalNote);
		return this.get(decisionId, actor.acls);
	}

	async reverseAutomatedPurchaseLimits(subjectId: UserID, paymentAt: Date, internalNote: string): Promise<void> {
		const active = await listActiveFeatureLimits(this.deps.repository, subjectId, 'purchases');
		const matching = active.filter(
			(decision) => decision.automation === 'automated' && decision.payment_at?.getTime() === paymentAt.getTime(),
		);
		for (const decision of matching) {
			await this.finish(decision, {outcome: 'reversed'}, SYSTEM_USER_ID, internalNote);
		}
	}

	async get(decisionId: bigint, acls: ReadonlySet<string>): Promise<EnforcementDecisionResponse> {
		const {repository, delivery} = this.deps;
		const decision = await this.requireDecision(decisionId);
		const [notes, evidence, subject, recipient, networkBlocks] = await Promise.all([
			repository.listNotes(decisionId),
			repository.getEvidence(decisionId),
			this.findUserSubject(decision),
			delivery.findRecipient(decision),
			this.deps.banManagementService.listDecisionNetworkBlocks(decisionId),
		]);
		const notice = loadDecisionNotice(this.deps, decision, {
			messages: evidence?.messages ?? [],
			channelKinds: evidence?.message_locations ?? null,
			networkBlocks,
			accountHasEmail: Boolean(recipient?.email),
			locale: 'en-US',
		});
		return toAdminDecisionResponse({decision, subject, notes, evidence, networkBlocks, notice: await notice, acls});
	}

	async list(query: EnforcementDecisionListQuery, acls: ReadonlySet<string>): Promise<EnforcementDecisionListResponse> {
		if (query.review_queue || query.subject_type === undefined || query.subject_id === undefined) {
			const queued = await this.deps.repository.listReviewQueue();
			return {decisions: queued.slice(0, query.limit).map((decision) => toDecisionSummary(decision, null, acls))};
		}
		const decisions = await this.deps.repository.listBySubject(
			query.subject_type,
			query.subject_id,
			query.limit,
			query.before,
		);
		const subject =
			query.subject_type === 'user'
				? await this.deps.apiContext.services.users.findUnique(createUserID(query.subject_id))
				: null;
		return {decisions: decisions.map((decision) => toDecisionSummary(decision, subject, acls))};
	}

	async listNetworkBlockCandidates(userId: bigint): Promise<NetworkBlockCandidatesResponse> {
		const {users} = this.deps.apiContext.services;
		const user = await users.findUnique(createUserID(userId));
		if (!user) {
			throw new UnknownUserError();
		}
		return listNetworkBlockCandidates(users, user);
	}

	async addNote(decisionId: bigint, body: string, actor: EnforcementActor) {
		await this.requireDecision(decisionId);
		const note = {
			decision_id: decisionId,
			note_id: await this.deps.apiContext.services.snowflake.generate(),
			author_id: actor.adminUserId,
			body,
			created_at: new Date(),
		};
		await this.deps.repository.addNote(note);
		await this.deps.auditService.createAuditLog({
			adminUserId: actor.adminUserId,
			targetType: 'enforcement_decision',
			targetId: decisionId,
			action: 'annotate_enforcement_decision',
			auditLogReason: actor.internalNote,
			metadata: new Map([
				['decision_id', decisionId.toString()],
				['note_id', note.note_id.toString()],
			]),
		});
		return toNoteResponse(note);
	}

	private async subjectFields(subject: EnforcementSubject): Promise<DecisionSubjectFields> {
		if (subject.type === 'guild') {
			const {guild} = subject;
			return {subject_type: 'guild', subject_id: guild.id, subject_name: guild.name, recipient_user_id: guild.ownerId};
		}
		const {user} = subject;
		return {
			subject_type: 'user',
			subject_id: user.id,
			subject_name: user.isBot ? user.username : null,
			recipient_user_id: user.isBot ? await this.deps.delivery.findBotOwner(user.id) : user.id,
		};
	}

	private async persist(
		subject: DecisionSubjectFields,
		fields: DecisionFields,
		evidence: {
			messages: ReadonlyArray<IARMessageContextRow>;
			channelKinds: ReadonlyMap<bigint, EvidenceChannelKind>;
			profileValues: ReadonlyMap<string, string> | null;
		},
		firstNote: string,
		applyState: (decisionId: bigint) => Promise<void>,
	): Promise<EnforcementDecisionRow> {
		const {repository} = this.deps;
		const {snowflake} = this.deps.apiContext.services;
		const decisionId = await snowflake.generate();
		const now = new Date();
		const decision: EnforcementDecisionRow = {
			...subject,
			...fields,
			decision_id: decisionId,
			created_at: now,
			status: 'active',
			end_outcome: null,
			ended_at: null,
			ended_by: null,
			end_statement: null,
			notice_status: 'pending',
			notice_channels: null,
			notice_locale: null,
			notice_text: null,
			notice_sent_at: null,
		};
		const note: EnforcementDecisionNoteRow = {
			decision_id: decisionId,
			note_id: await snowflake.generate(),
			author_id: fields.decided_by,
			body: firstNote,
			created_at: now,
		};
		const hasEvidence = evidence.messages.length > 0 || (evidence.profileValues?.size ?? 0) > 0;
		await repository.create({
			decision,
			evidence: hasEvidence
				? {
						decision_id: decisionId,
						messages: evidence.messages.length > 0 ? [...evidence.messages] : null,
						message_locations: evidence.channelKinds.size > 0 ? new Map(evidence.channelKinds) : null,
						profile_values: evidence.profileValues?.size ? new Map(evidence.profileValues) : null,
					}
				: null,
			firstNote: note,
		});
		try {
			await applyState(decisionId);
		} catch (error) {
			if (await this.subjectNamesDecision(decision)) {
				Logger.error(
					{decisionId: decisionId.toString(), error},
					'An enforcement state change failed after the account was updated, so the decision is kept',
				);
			} else {
				await repository.delete(decision).catch((deleteError: unknown) => {
					Logger.error(
						{decisionId: decisionId.toString(), deleteError},
						'Failed to delete an enforcement decision whose state change failed',
					);
				});
			}
			throw error;
		}
		return decision;
	}

	private async finish(
		decision: EnforcementDecisionRow,
		request: EnforcementDecisionEndRequest,
		endedBy: UserID,
		internalNote: string | null,
	): Promise<void> {
		if (decision.status === 'ended' && !(decision.end_outcome === 'expired' && request.outcome === 'reversed')) {
			throw new EnforcementDecisionEndedError();
		}
		if (request.outcome === 'lifted' && !UNDOABLE_KINDS.has(decision.kind)) {
			throw InputValidationError.fromCode('outcome', ValidationErrorCodes.ENFORCEMENT_OUTCOME_NOT_ALLOWED);
		}
		const inForce = await this.undo(decision, request, endedBy, internalNote);
		const ended: EnforcementDecisionRow = {
			...decision,
			status: 'ended',
			end_outcome: request.outcome,
			ended_at: new Date(),
			ended_by: endedBy,
			end_statement: request.statement ?? null,
		};
		await this.deps.repository.end(decision.decision_id, {
			outcome: request.outcome,
			endedAt: ended.ended_at!,
			endedBy,
			statement: ended.end_statement,
		});
		if (decision.automation === 'automated') {
			await this.deps.repository.removeFromReviewQueue([decision.decision_id]);
		}
		await this.deps.delivery.deliverEnded(ended, request.outcome, inForce);
		await this.deps.auditService.createAuditLog({
			adminUserId: endedBy,
			targetType: 'enforcement_decision',
			targetId: decision.decision_id,
			action: 'end_enforcement_decision',
			auditLogReason: internalNote,
			metadata: new Map([
				['decision_id', decision.decision_id.toString()],
				['kind', decision.kind],
				['outcome', request.outcome],
				['subject_id', decision.subject_id.toString()],
			]),
		});
	}

	private async undo(
		decision: EnforcementDecisionRow,
		request: EnforcementDecisionEndRequest,
		actorId: UserID,
		internalNote: string | null,
	): Promise<boolean> {
		const subjectId = createUserID(decision.subject_id);
		switch (decision.kind) {
			case 'suspension':
			case 'closure': {
				const user = await this.requireUserSubject(decision);
				if (user.enforcementDecisionId !== decision.decision_id) {
					if (request.outcome !== 'reversed' && user.enforcementDecisionId !== null) {
						throw new EnforcementDecisionReplacedError();
					}
					if (request.outcome !== 'reversed') {
						throw InputValidationError.fromCode('outcome', ValidationErrorCodes.ENFORCEMENT_OUTCOME_NOT_ALLOWED);
					}
					await this.deps.banManagementService.removeDecisionNetworkBlocks([decision.decision_id]);
					return false;
				}
				if (decision.kind === 'suspension') {
					await liftSuspension(this.deps, user);
					await this.deps.banManagementService.removeDecisionNetworkBlocks([decision.decision_id]);
					await emitAdminAction(actorId, subjectId, 'unban');
					return true;
				}
				if (user.deletionStartedAt) {
					throw new EnforcementDeletionStartedError();
				}
				await cancelClosure(this.deps, user);
				await this.deps.banManagementService.removeDecisionNetworkBlocks([decision.decision_id]);
				const {rule} = ENFORCEMENT_REASONS[decision.reason_code];
				if (ENFORCEMENT_RULES[rule].barsNewAccounts && user.email) {
					await this.deps.banManagementService.unbanEmail({email: user.email}, actorId, internalNote);
				}
				await emitAdminAction(actorId, subjectId, 'cancel_deletion');
				return true;
			}
			case 'feature_limit': {
				const limit = decision.feature_limit!;
				const user = await this.requireUserSubject(decision);
				const holding = await findHoldingFeatureLimit(this.deps.repository, subjectId, limit, decision.decision_id);
				if (!holding) {
					await setFeatureLimit(this.deps, user, limit, false, actorId);
				}
				return true;
			}
			default:
				return true;
		}
	}

	private async subjectNamesDecision(decision: EnforcementDecisionRow): Promise<boolean> {
		if (decision.subject_type !== 'user' || !isAccountKind(decision.kind)) return false;
		try {
			const user = await this.findUserSubject(decision);
			return user?.enforcementDecisionId === decision.decision_id;
		} catch {
			return true;
		}
	}

	private async takeOverNetworkBlocks(
		replacedDecisionId: bigint,
		decision: EnforcementDecisionRow,
		keep: boolean,
	): Promise<Array<BannedIpEntry>> {
		const {banManagementService} = this.deps;
		try {
			if (keep) {
				return await banManagementService.relinkDecisionNetworkBlocks(replacedDecisionId, decision.decision_id);
			}
			await banManagementService.removeDecisionNetworkBlocks([replacedDecisionId]);
		} catch (error) {
			Logger.error(
				{decisionId: decision.decision_id.toString(), replacedDecisionId: replacedDecisionId.toString(), error},
				'Failed to move the network blocks of a replaced decision',
			);
		}
		return [];
	}

	private async clearReviewQueueFor(subjectId: UserID): Promise<void> {
		try {
			const queued = await this.deps.repository.listReviewQueue();
			await this.deps.repository.removeFromReviewQueue(
				queued.filter((decision) => decision.subject_id === subjectId).map((decision) => decision.decision_id),
			);
		} catch (error) {
			Logger.warn({subjectId: subjectId.toString(), error}, 'Failed to clear the review queue for a subject');
		}
	}

	private async requireDecision(decisionId: bigint): Promise<EnforcementDecisionRow> {
		const decision = await this.deps.repository.get(decisionId);
		if (!decision) {
			throw new UnknownEnforcementDecisionError();
		}
		return decision;
	}

	private async findUserSubject(decision: EnforcementDecisionRow): Promise<User | null> {
		if (decision.subject_type !== 'user') return null;
		return this.deps.apiContext.services.users.findUnique(createUserID(decision.subject_id));
	}

	private async requireUserSubject(decision: EnforcementDecisionRow): Promise<User> {
		const user = await this.findUserSubject(decision);
		if (!user) {
			throw new UnknownUserError();
		}
		return user;
	}

	private async validate({
		subject,
		kind,
		input,
		endsAt,
		limit,
		removeEvidence,
		removal,
		blockNetworks,
		actor,
	}: RecordDecisionParams): Promise<{
		reports: Array<IARSubmission>;
		evidence: Array<IARMessageContextRow>;
		removedMessageCount: number | null;
		internalNote: string;
		networkPlan: NetworkBlockPlan | null;
	}> {
		const reportIds = input.report_ids ?? [];
		const explicitEvidence = input.evidence ?? [];
		requireGrantedAdminACLs(actor.acls, [
			...(explicitEvidence.length > 0 ? [AdminACLs.MESSAGE_LOOKUP] : []),
			...(reportIds.length > 0 ? [AdminACLs.REPORT_VIEW, AdminACLs.REPORT_RESOLVE] : []),
			...(removeEvidence ? [AdminACLs.MESSAGE_SHRED] : []),
			...(blockNetworks ? [AdminACLs.BAN_IP_ADD, AdminACLs.USER_VIEW_IP] : []),
		]);
		if (!actor.internalNote) {
			throw InputValidationError.fromCode(
				'X-Audit-Log-Reason',
				ValidationErrorCodes.ENFORCEMENT_INTERNAL_NOTE_REQUIRED,
			);
		}
		if (removal?.type === 'messages' && explicitEvidence.length > 0) {
			throw InputValidationError.fromCode('decision.evidence', ValidationErrorCodes.ENFORCEMENT_EVIDENCE_NOT_ALLOWED);
		}
		const reason = ENFORCEMENT_REASONS[input.reason_code];
		if (
			!isEnforcementKindAllowed(input.reason_code, kind) ||
			(subject.type === 'user' && subject.user.isBot && !isAccountKind(kind)) ||
			(removal?.type === 'all_messages' && reason.handling === 'restricted')
		) {
			throw InputValidationError.fromCode('decision.reason_code', ValidationErrorCodes.ENFORCEMENT_KIND_NOT_ALLOWED);
		}
		if (kind === 'feature_limit' && (!limit || !isEnforcementLimitAllowed(input.reason_code, limit))) {
			throw InputValidationError.fromCode('limit', ValidationErrorCodes.ENFORCEMENT_LIMIT_NOT_ALLOWED);
		}
		if (blockNetworks && (subject.type !== 'user' || !canBlockNetworks(kind, endsAt))) {
			throw InputValidationError.fromCode('block_networks', ValidationErrorCodes.ENFORCEMENT_NETWORK_BLOCK_NOT_ALLOWED);
		}
		if (input.source === 'report' && reportIds.length === 0) {
			throw InputValidationError.fromCode('decision.report_ids', ValidationErrorCodes.ENFORCEMENT_REPORT_REQUIRED);
		}
		const reports = await this.loadReports(reportIds, subject);
		const removed = removal?.type === 'messages' ? removal.entries : [];
		const snapshotted = reason.handling === 'restricted' ? removed.length : EVIDENCE_LIMIT;
		const evidence = await collectEvidence({
			subject: evidenceSubject(subject),
			messages: removed.length > 0 ? removed.slice(0, snapshotted) : explicitEvidence,
			path: removed.length > 0 ? 'entries' : 'decision.evidence',
			reports,
			channelRepository: this.deps.channelRepository,
		});
		if (removeEvidence && evidence.length === 0) {
			throw InputValidationError.fromCode('remove_evidence', ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED);
		}
		if (reason.handling === 'restricted') {
			this.validateRestricted(input, kind, endsAt ?? null, evidence.length);
		} else if (evidence.length === 0 && !input.statement && kind !== 'profile_reset') {
			throw InputValidationError.fromCode('decision', ValidationErrorCodes.ENFORCEMENT_FACTS_REQUIRED);
		}
		if (reason.rule === 'copyright' && !input.rights_holder) {
			throw InputValidationError.fromCode(
				'decision.rights_holder',
				ValidationErrorCodes.ENFORCEMENT_RIGHTS_HOLDER_REQUIRED,
			);
		}
		if (reason.rule !== 'copyright' && input.rights_holder) {
			throw InputValidationError.fromCode(
				'decision.rights_holder',
				ValidationErrorCodes.ENFORCEMENT_RIGHTS_HOLDER_NOT_ALLOWED,
			);
		}
		const networkPlan =
			blockNetworks && subject.type === 'user'
				? await planNetworkBlocks(this.deps.apiContext.services.users, subject.user, blockNetworks)
				: null;
		const removedMessageCount =
			removal?.type === 'messages'
				? evidence.length + (await this.countOwnMessages(subject, removed.slice(snapshotted)))
				: removeEvidence
					? evidence.length
					: null;
		return {reports, evidence, removedMessageCount, internalNote: actor.internalNote, networkPlan};
	}

	private async countOwnMessages(
		subject: EnforcementSubject,
		entries: ReadonlyArray<MessageReference>,
	): Promise<number> {
		if (subject.type !== 'user') return 0;
		let count = 0;
		for (const entry of entries) {
			const message = await this.deps.channelRepository.messages.getMessage(
				createChannelID(entry.channel_id),
				createMessageID(entry.message_id),
			);
			if (message?.authorId === subject.user.id) count += 1;
		}
		return count;
	}

	private async blockNetworks(
		decision: EnforcementDecisionRow,
		plan: NetworkBlockPlan,
	): Promise<{blocked: Array<BannedIpEntry>; skipped: Array<SkippedNetwork>}> {
		if (plan.ips.length === 0) return {blocked: [], skipped: plan.skipped};
		try {
			const {blocked, alreadyBlocked} = await this.deps.banManagementService.blockNetworksForDecision(
				plan.ips,
				decision.decision_id,
				plan.expiresAt,
			);
			return {
				blocked,
				skipped: [...plan.skipped, ...alreadyBlocked.map((ip) => ({ip, reason: 'already_blocked' as const}))],
			};
		} catch (error) {
			Logger.error({decisionId: decision.decision_id.toString(), error}, 'Failed to block the networks of a decision');
			return {blocked: [], skipped: plan.skipped};
		}
	}

	private validateRestricted(
		input: EnforcementDecisionInput,
		kind: EnforcementKind,
		endsAt: Date | null,
		evidenceCount: number,
	): void {
		if (input.statement) {
			throw InputValidationError.fromCode('decision.statement', ValidationErrorCodes.ENFORCEMENT_STATEMENT_NOT_ALLOWED);
		}
		if (evidenceCount === 0) {
			throw InputValidationError.fromCode('decision.evidence', ValidationErrorCodes.ENFORCEMENT_EVIDENCE_REQUIRED);
		}
		if (kind === 'suspension' && endsAt !== null) {
			throw InputValidationError.fromCode('duration_hours', ValidationErrorCodes.ENFORCEMENT_PERMANENT_REQUIRED);
		}
		if (!CONFIRMATIONS.every((confirmation) => input.confirmations?.includes(confirmation))) {
			throw InputValidationError.fromCode(
				'decision.confirmations',
				ValidationErrorCodes.ENFORCEMENT_CONFIRMATIONS_REQUIRED,
			);
		}
	}

	private async loadReports(
		reportIds: ReadonlyArray<bigint>,
		subject: EnforcementSubject,
	): Promise<Array<IARSubmission>> {
		const subjectId = subject.type === 'user' ? subject.user.id : subject.guild.id;
		const reports: Array<IARSubmission> = [];
		for (const [index, reportId] of [...new Set(reportIds)].entries()) {
			const report = await this.deps.reportService.getReport(createReportID(reportId)).catch((error: unknown) => {
				if (error instanceof UnknownReportError) return null;
				throw error;
			});
			const reported = subject.type === 'user' ? report?.reportedUserId : report?.reportedGuildId;
			if (!report || reported !== subjectId) {
				throw InputValidationError.fromCode(
					`decision.report_ids.${index}`,
					ValidationErrorCodes.ENFORCEMENT_REPORT_MISMATCH,
				);
			}
			reports.push(report);
		}
		return reports;
	}

	private async queueEvidenceRemoval(
		decision: EnforcementDecisionRow,
		evidence: ReadonlyArray<IARMessageContextRow>,
		adminUserId: UserID,
	): Promise<void> {
		const entries = evidence.flatMap((message) =>
			message.channel_id ? [{channel_id: message.channel_id, message_id: message.message_id}] : [],
		);
		try {
			await queueMessageShredJob(this.deps.apiContext, adminUserId, decision.subject_id, entries);
		} catch (error) {
			Logger.error(
				{decisionId: decision.decision_id.toString(), error},
				'Failed to queue the removal of enforcement evidence',
			);
		}
	}

	private async resolveReports(
		outcomes: ReadonlyArray<{report: IARSubmission; outcome: ReportItemOutcome | null}>,
		actor: EnforcementActor,
		internalNote: string,
	): Promise<void> {
		for (const {report, outcome} of outcomes) {
			if (report.status !== ReportStatus.PENDING || outcome === null) continue;
			try {
				await this.deps.adminReportService.resolveReport(report.reportId, {
					adminUserId: actor.adminUserId,
					publicComment: null,
					auditLogReason: internalNote,
					notifyReporter: true,
					resolution: 'actioned',
					outcome,
				});
			} catch (error) {
				Logger.warn({reportId: report.reportId.toString(), error}, 'Failed to resolve a report linked to a decision');
			}
		}
	}

	private async deliverNotice(
		decision: EnforcementDecisionRow,
		evidence: ReadonlyArray<IARMessageContextRow>,
		channelKinds: ReadonlyMap<bigint, EvidenceChannelKind>,
		networksBlockedUntil: Date | null,
	): Promise<EnforcementDecisionRow> {
		const {repository, delivery, channelRepository, guildRepository} = this.deps;
		try {
			const locations = await resolveEvidenceLocations(evidence, channelKinds, channelRepository, guildRepository);
			const notice = await delivery.deliver(decision, locations, networksBlockedUntil);
			if (!notice) return decision;
			await repository.setNotice(decision.decision_id, notice);
			return {
				...decision,
				notice_status: notice.status,
				notice_channels: notice.channels.size > 0 ? new Set(notice.channels) : null,
				notice_locale: notice.locale,
				notice_text: notice.text,
				notice_sent_at: notice.sentAt,
			};
		} catch (error) {
			Logger.error({decisionId: decision.decision_id.toString(), error}, 'Failed to deliver an enforcement notice');
			return decision;
		}
	}
}
