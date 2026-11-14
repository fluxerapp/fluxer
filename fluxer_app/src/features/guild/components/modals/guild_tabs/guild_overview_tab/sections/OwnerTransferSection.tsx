// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getOverrideMemberLabel,
	selectOverrideMembers,
} from '@app/features/app/components/dialogs/shared/AddOverrideMemberSearch';
import Authentication from '@app/features/auth/state/Authentication';
import {SettingsSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/components/GuildOverviewTabSettingsSection';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/sections/OwnerTransferSection.module.css';
import {TransferOwnershipModal} from '@app/features/guild/components/modals/TransferOwnershipModal';
import Guilds from '@app/features/guild/state/Guilds';
import {
	TRANSFER_COMMUNITY_OWNERSHIP_DESCRIPTOR,
	TRANSFER_OWNERSHIP_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {UserCard} from '@app/features/user/components/UserCard';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect, useState} from 'react';

const OWNER_TRANSFER_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Make another member the owner of this community. You cannot undo this.',
	comment: 'Description of the community ownership transfer section in community settings.',
});
const SEARCH_MEMBERS_DESCRIPTOR = msg({
	message: 'Search members',
	comment: 'Placeholder of the member search in the community ownership transfer section.',
});

const MEMBER_LIMIT = 10;
const SEARCH_DEBOUNCE_MS = 300;

interface MemberOption extends ComboboxOption<string | null> {
	member: GuildMember;
}

const canTransferOwnershipTo = (member: GuildMember, currentUserId: string | null): boolean =>
	!member.user.bot && member.user.id !== currentUserId;

const USER_ID_PATTERN = /^\d{15,20}$/;

export const OwnerTransferSection: React.FC<{
	guildId: string;
}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const [query, setQuery] = useState('');
	const [searchIds, setSearchIds] = useState<Array<string>>([]);
	const [selectedMemberId, setSelectedMemberId] = useState<string | null>(null);
	const currentUserId = Authentication.currentUserId;
	useEffect(() => {
		const trimmed = query.trim();
		if (!trimmed) {
			setSearchIds([]);
			return;
		}
		const context = MemberSearch.getSearchContext((results) => {
			setSearchIds(results.map((result) => result.id));
		}, MEMBER_LIMIT);
		context.beginSearch(trimmed, {guild: guildId});
		const timer = setTimeout(() => {
			if (!GuildMembers.isGuildFullyLoaded(guildId)) {
				void MemberSearch.fetchMembersInBackground(trimmed, [guildId], guildId);
			}
			if (USER_ID_PATTERN.test(trimmed) && GuildMembers.getMember(guildId, trimmed) == null) {
				GuildMembers.fetchMembers(guildId, {userIds: [trimmed]}).catch(() => {});
			}
		}, SEARCH_DEBOUNCE_MS);
		return () => {
			clearTimeout(timer);
			context.destroy();
		};
	}, [query, guildId]);
	if (guild == null || guild.ownerId !== currentUserId) {
		return null;
	}
	const isEligible = (member: GuildMember | null): member is GuildMember =>
		member != null && canTransferOwnershipTo(member, currentUserId);
	const members = selectOverrideMembers({
		cachedMembers: GuildMembers.getMembers(guildId).filter(isEligible),
		workerMemberIds: searchIds,
		resolveMember: (userId) => {
			const member = GuildMembers.getMember(guildId, userId);
			return isEligible(member) ? member : null;
		},
		excludedIds: new Set(),
		guildId,
		query,
		limit: MEMBER_LIMIT,
	});
	const trimmedQuery = query.trim();
	const idMember = USER_ID_PATTERN.test(trimmedQuery) ? GuildMembers.getMember(guildId, trimmedQuery) : null;
	const selectedMember = selectedMemberId == null ? null : GuildMembers.getMember(guildId, selectedMemberId);
	const optionMembers = [selectedMember, idMember]
		.filter(isEligible)
		.filter((member) => !members.some((candidate) => candidate.user.id === member.user.id))
		.concat(members);
	const options: Array<MemberOption> = optionMembers.map((member) => ({
		value: member.user.id,
		label: getOverrideMemberLabel(member, guildId),
		member,
	}));
	const beginTransferOwnership = () => {
		if (selectedMember == null) {
			return;
		}
		ModalCommands.push(
			modal(() => (
				<TransferOwnershipModal
					guildId={guildId}
					targetUser={selectedMember.user}
					data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.begin-transfer-ownership.transfer-ownership-modal"
				/>
			)),
		);
	};
	return (
		<SettingsSection
			title={i18n._(TRANSFER_COMMUNITY_OWNERSHIP_DESCRIPTOR)}
			description={i18n._(OWNER_TRANSFER_DESCRIPTION_DESCRIPTOR)}
			data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.settings-section"
		>
			<div className={styles.search} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.search">
				<Combobox<string | null, MemberOption>
					value={selectedMemberId}
					options={options}
					onChange={setSelectedMemberId}
					onInputValueChange={setQuery}
					filterOption={() => true}
					isClearable={true}
					blurInputOnSelect={false}
					placeholder={i18n._(SEARCH_MEMBERS_DESCRIPTOR)}
					aria-label={i18n._(SEARCH_MEMBERS_DESCRIPTOR)}
					renderOption={(option) => (
						<span
							className={styles.option}
							data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.option"
						>
							<Avatar
								user={option.member.user}
								size={24}
								guildId={guildId}
								data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.option-avatar"
							/>
							<span
								className={styles.optionLabel}
								data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.option-label"
							>
								{option.label}
							</span>
						</span>
					)}
					data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.member-combobox"
				/>
				{selectedMember != null && (
					<UserCard
						user={selectedMember.user}
						guildId={guildId}
						data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.user-card"
					>
						<Button
							variant="danger"
							leftIcon={
								<WarningIcon
									size={16}
									weight="fill"
									data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.warning-icon"
								/>
							}
							onClick={beginTransferOwnership}
							data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.button.begin-transfer-ownership"
						>
							{i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR)}
						</Button>
					</UserCard>
				)}
			</div>
		</SettingsSection>
	);
});
