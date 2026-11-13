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
import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Input} from '@app/features/ui/components/form/FormInput';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {UserCard} from '@app/features/user/components/UserCard';
import {isProbablyAValidSnowflake} from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {IdentificationCardIcon, MagnifyingGlassIcon, WarningIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef, useState} from 'react';

const SEARCH_MEMBERS_DESCRIPTOR = msg({
	message: 'Search members',
	comment: 'Placeholder of the member search in the community ownership transfer section.',
});
const NO_MATCHING_MEMBERS_DESCRIPTOR = msg({
	message: 'No members match that search',
	comment: 'Empty state of the member search in the community ownership transfer section.',
});
const USER_ID_PLACEHOLDER_DESCRIPTOR = msg({
	message: '(Optional) Enter a specific user ID to select a member',
	comment:
		'Placeholder of the user ID field in the community ownership transfer section. Explains that entering an ID can also selects a member.',
});
const USER_ID_NOT_FOUND_DESCRIPTOR = msg({
	message: 'No member of this community has that ID',
	comment: 'Shown in the community ownership transfer section when the entered user ID cannot be found.',
});
const USER_ID_NOT_ELIGIBLE_DESCRIPTOR = msg({
	message: 'This member is ineligble for selection',
	comment:
		'Shown in the community ownership transfer section when the entered user ID belongs to a bot or to the current owner.',
});

const MEMBER_LIMIT = 10;
const SEARCH_DEBOUNCE_MS = 300;

const renderUserItem = (member: GuildMember, guildId: string) => (
	<>
		<Avatar
			user={member.user}
			size={24}
			guildId={guildId}
			data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.render-user-item.avatar"
		/>
		<span
			className={styles.resultLabel}
			data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.render-user-item.result-label"
		>
			{getOverrideMemberLabel(member, guildId)}
		</span>
	</>
);

const canTransferOwnershipTo = (member: GuildMember, currentUserId: string | null): boolean =>
	!member.user.bot && member.user.id !== currentUserId;

export const OwnerTransferSection: React.FC<{
	guildId: string;
}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const inputRef = useRef<HTMLInputElement | null>(null);
	const resultsRef = useRef<HTMLDivElement | null>(null);
	const [query, setQuery] = useState('');
	const [searchIds, setSearchIds] = useState<Array<string>>([]);
	const [isFocused, setIsFocused] = useState(false);
	const [selectedMemberId, setSelectedMemberId] = useState<string | null>(null);
	const [isIdSelection, setIsIdSelection] = useState(false);
	const [userIdQuery, setUserIdQuery] = useState('');
	const [userIdError, setUserIdError] = useState<'not-found' | 'ineligble' | null>(null);
	const currentUserId = Authentication.currentUserId;
	const isOwner = guild?.ownerId === currentUserId;
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
		const timer = GuildMembers.isGuildFullyLoaded(guildId)
			? null
			: setTimeout(() => void MemberSearch.fetchMembersInBackground(trimmed, [guildId], guildId), SEARCH_DEBOUNCE_MS);
		return () => {
			if (timer) clearTimeout(timer);
			context.destroy();
		};
	}, [query, guildId]);
	useEffect(() => {
		const trimmed = userIdQuery.trim();
		setUserIdError(null);
		if (!isProbablyAValidSnowflake(trimmed) || trimmed === selectedMemberId) {
			return;
		}
		const cachedMember = GuildMembers.getMember(guildId, trimmed);
		if (cachedMember) {
			if (!canTransferOwnershipTo(cachedMember, currentUserId)) {
				setUserIdError('ineligble');
				return;
			}
			setSelectedMemberId(cachedMember.user.id);
			setIsIdSelection(true);
			setQuery(getOverrideMemberLabel(cachedMember, guildId));
			return;
		}
		let cancelled = false;
		const timer = setTimeout(() => {
			void GuildMembers.fetchMembers(guildId, {userIds: [trimmed]})
				.then(() => {
					if (cancelled) return;
					const member = GuildMembers.getMember(guildId, trimmed);
					if (member == null) {
						setUserIdError('not-found');
						return;
					}
					if (!canTransferOwnershipTo(member, currentUserId)) {
						setUserIdError('ineligble');
						return;
					}
					setSelectedMemberId(member.user.id);
					setIsIdSelection(true);
					setQuery(getOverrideMemberLabel(member, guildId));
				})
				.catch(() => {
					if (!cancelled) {
						setUserIdError('not-found');
					}
				});
		}, SEARCH_DEBOUNCE_MS);
		return () => {
			cancelled = true;
			clearTimeout(timer);
		};
	}, [userIdQuery, guildId, selectedMemberId, currentUserId]);
	useEffect(() => {
		if (!isFocused) return;
		const frame = requestAnimationFrame(() => {
			const container = resultsRef.current;
			if (container == null) return;
			const selectedRow = container.querySelector<HTMLElement>('[data-selected="true"]');
			(selectedRow ?? container).scrollIntoView({
				block: selectedRow ? 'nearest' : 'end',
				behavior: 'smooth',
			});
		});
		return () => cancelAnimationFrame(frame);
	}, [isFocused]);
	const members = selectOverrideMembers({
		cachedMembers: GuildMembers.getMembers(guildId),
		workerMemberIds: searchIds,
		resolveMember: (userId) => GuildMembers.getMember(guildId, userId),
		excludedIds: new Set(),
		guildId,
		query,
		limit: MEMBER_LIMIT,
	});
	const selectableMembers = members.filter((member) => canTransferOwnershipTo(member, currentUserId));
	const selectedMember = selectedMemberId == null ? null : GuildMembers.getMember(guildId, selectedMemberId);
	const isListVisible = isFocused;
	const selectMember = (member: GuildMember) => {
		setSelectedMemberId(member.user.id);
		setIsIdSelection(false);
		setUserIdQuery(member.user.id);
		setUserIdError(null);
		setQuery(getOverrideMemberLabel(member, guildId));
		setIsFocused(false);
		inputRef.current?.blur();
	};
	const clearSelection = () => {
		setSelectedMemberId(null);
		setIsIdSelection(false);
		setUserIdQuery('');
		setUserIdError(null);
		setQuery('');
	};
	const beginTransferOwnership = () => {
		if (selectedMember == null) {
			return;
		}
		ModalCommands.push(
			modal(() => (
				<TransferOwnershipModal
					guildId={guildId}
					targetUser={selectedMember.user}
					targetMember={selectedMember}
					data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.begin-transfer-ownership.transfer-ownership-modal"
				/>
			)),
		);
	};
	return (
		<SettingsSection
			title={
				<span className={styles.title} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.title">
					<WarningIcon
						className={styles.titleIcon}
						size={14}
						weight="fill"
						data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.title-icon"
					/>
					<Trans>Transfer Community Ownership</Trans>
				</span>
			}
			description={<Trans>Change the ownership of this community. Once completed, this is irreversible.</Trans>}
			data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.settings-section"
		>
			<div className={styles.search} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.search">
				<Input
					ref={inputRef}
					type="text"
					value={query}
					onChange={(event) => {
						setSelectedMemberId(null);
						setIsIdSelection(false);
						setQuery(event.target.value);
					}}
					onFocus={() => setIsFocused(true)}
					onBlur={() => setIsFocused(false)}
					placeholder={i18n._(SEARCH_MEMBERS_DESCRIPTOR)}
					autoComplete="off"
					disabled={!isOwner}
					leftIcon={
						<MagnifyingGlassIcon
							size={16}
							weight="bold"
							data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.magnifying-glass-icon"
						/>
					}
					rightElement={
						query.length > 0 ? (
							<FocusRing offset={-2} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.focus-ring">
								<button
									type="button"
									onMouseDown={(event) => event.preventDefault()}
									onClick={clearSelection}
									className={styles.clearButton}
									aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
									data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.clear-button.clear-selection"
								>
									<XIcon
										size={14}
										weight="bold"
										data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.x-icon"
									/>
								</button>
							</FocusRing>
						) : undefined
					}
					data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.input.set-selected-member-id.text"
				/>
				{isListVisible && (
					<div
						className={styles.results}
						ref={resultsRef}
						data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.results"
					>
						{selectableMembers.length > 0 ? (
							selectableMembers.map((member) => (
								<button
									key={member.user.id}
									type="button"
									onMouseDown={(event) => {
										event.preventDefault();
										selectMember(member);
									}}
									className={clsx(styles.result, member.user.id === selectedMemberId && styles.resultSelected)}
									data-selected={member.user.id === selectedMemberId ? 'true' : undefined}
									data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.result.prevent-default.button"
								>
									{renderUserItem(member, guildId)}
								</button>
							))
						) : (
							<p className={styles.empty} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.empty">
								{i18n._(NO_MATCHING_MEMBERS_DESCRIPTOR)}
							</p>
						)}
					</div>
				)}
				<div className={styles.finder} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.finder">
					{selectedMember != null && (
						<UserCard
							user={selectedMember.user}
							guildId={guildId}
							data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.user-card"
						>
							{!isIdSelection && (
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
									<Trans>Transfer Ownership</Trans>
								</Button>
							)}
						</UserCard>
					)}
					<Input
						type="text"
						value={userIdQuery}
						onChange={(event) => setUserIdQuery(event.target.value)}
						placeholder={i18n._(USER_ID_PLACEHOLDER_DESCRIPTOR)}
						aria-label={i18n._(USER_ID_PLACEHOLDER_DESCRIPTOR)}
						autoComplete="off"
						disabled={!isOwner}
						leftIcon={
							<IdentificationCardIcon
								size={16}
								weight="bold"
								data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.identification-card-icon"
							/>
						}
						data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.input.set-user-id-query.text"
					/>
					{userIdError != null && (
						<p className={styles.invalid} data-flx="guild.guild-tabs.guild-overview-tab.owner-transfer-section.invalid">
							{userIdError === 'ineligble'
								? i18n._(USER_ID_NOT_ELIGIBLE_DESCRIPTOR)
								: i18n._(USER_ID_NOT_FOUND_DESCRIPTOR)}
						</p>
					)}
				</div>
			</div>
		</SettingsSection>
	);
});
