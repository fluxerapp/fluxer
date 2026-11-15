// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository, type BannedIpEntry} from '@app/api/admin/AdminRepository';
import type {AdminBanManagementService} from '@app/api/admin/services/AdminBanManagementService';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {EnforcementDecisionRow, EvidenceChannelKind} from '@app/api/database/types/EnforcementTypes';
import type {IARMessageContextRow} from '@app/api/database/types/ReportTypes';
import {EnforcementDecisionRepository} from '@app/api/enforcement/EnforcementDecisionRepository';
import {resolveEvidenceLocations} from '@app/api/enforcement/EnforcementEvidence';
import {buildEnforcementNotice} from '@app/api/enforcement/EnforcementNotice';
import {renderNoticeText} from '@app/api/enforcement/EnforcementNoticeDelivery';
import {latestNetworkBlockExpiry} from '@app/api/enforcement/NetworkBlocks';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {getChannelRepository, getGuildRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import type {EnforcementNotice} from '@fluxer/schema/src/domains/auth/EnforcementNoticeSchemas';

interface NoticeLookupDeps {
	repository: EnforcementDecisionRepository;
	channelRepository: ChannelRepository;
	guildRepository: GuildRepository;
	banManagementService: Pick<AdminBanManagementService, 'listDecisionNetworkBlocks'>;
}

interface DecisionNoticeInputs {
	messages: ReadonlyArray<IARMessageContextRow>;
	channelKinds: ReadonlyMap<bigint, EvidenceChannelKind> | null;
	networkBlocks: ReadonlyArray<BannedIpEntry>;
	accountHasEmail: boolean;
	locale: string | null;
}

export async function loadDecisionNotice(
	deps: NoticeLookupDeps,
	decision: EnforcementDecisionRow,
	{messages, channelKinds, networkBlocks, accountHasEmail, locale}: DecisionNoticeInputs,
): Promise<EnforcementNotice> {
	return buildEnforcementNotice(
		{
			type: 'decision',
			decision,
			evidence: await resolveEvidenceLocations(messages, channelKinds, deps.channelRepository, deps.guildRepository),
			networksBlockedUntil: latestNetworkBlockExpiry(networkBlocks),
			accountHasEmail,
		},
		locale,
	);
}

export function createNoticeLookupDeps(): NoticeLookupDeps {
	const adminRepository = new AdminRepository();
	return {
		repository: new EnforcementDecisionRepository(),
		channelRepository: getChannelRepository(),
		guildRepository: getGuildRepository(),
		banManagementService: {listDecisionNetworkBlocks: (decisionId) => adminRepository.listDecisionIpBans(decisionId)},
	};
}

export async function loadAccountNotice(
	deps: NoticeLookupDeps,
	user: User,
	locale: string,
): Promise<EnforcementNotice> {
	const stored = user.enforcementDecisionId === null ? null : await deps.repository.get(user.enforcementDecisionId);
	if (!stored) {
		return buildEnforcementNotice({type: 'legacy', user}, locale);
	}
	const decision = stored.notice_sent_at === null ? {...stored, notice_sent_at: new Date()} : stored;
	const [evidence, networkBlocks] = await Promise.all([
		deps.repository.getEvidence(decision.decision_id),
		deps.banManagementService.listDecisionNetworkBlocks(decision.decision_id),
	]);
	const notice = await loadDecisionNotice(deps, decision, {
		messages: evidence?.messages ?? [],
		channelKinds: evidence?.message_locations ?? null,
		networkBlocks,
		accountHasEmail: user.email !== null,
		locale,
	});
	if (decision !== stored) {
		await deps.repository.setNotice(decision.decision_id, {
			status: 'sent',
			channels: new Set(['sign_in']),
			locale,
			text: renderNoticeText(decision, notice, locale),
			sentAt: decision.notice_sent_at,
		});
	}
	return notice;
}
