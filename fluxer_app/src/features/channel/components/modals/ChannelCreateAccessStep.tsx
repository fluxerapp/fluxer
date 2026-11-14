// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getOverrideMemberLabel,
	selectOverrideMembers,
} from '@app/features/app/components/dialogs/shared/AddOverrideMemberSearch';
import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import styles from '@app/features/channel/components/modals/ChannelCreateModal.module.css';
import type {ChannelCreateOverwrite} from '@app/features/channel/utils/ChannelCreateModalUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {MEMBERS_DESCRIPTOR, ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Input} from '@app/features/ui/components/form/FormInput';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';

const SEARCH_MEMBERS_OR_ROLES_DESCRIPTOR = msg({
	message: 'Search members or roles',
	comment: 'Placeholder of the search box in the add members or roles step of the create channel modal.',
});
const ACCESS_HINT_DESCRIPTOR = msg({
	message: 'Only the members and roles you pick, plus admins, can view this channel.',
	comment: 'Hint under the search box in the add members or roles step of the create channel modal.',
});
const NO_MATCHES_DESCRIPTOR = msg({
	message: 'No matching members or roles',
	comment: 'Empty state in the add members or roles step of the create channel modal.',
});

const MEMBER_LIMIT = 25;
const SEARCH_DEBOUNCE_MS = 300;

function overwriteKey(entry: ChannelCreateOverwrite): string {
	return `${entry.type}:${entry.id}`;
}

interface ChannelCreateAccessStepProps {
	guildId: string;
	selected: ReadonlyArray<ChannelCreateOverwrite>;
	onChange: (selected: Array<ChannelCreateOverwrite>) => void;
}

export const ChannelCreateAccessStep = observer(({guildId, selected, onChange}: ChannelCreateAccessStepProps) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const [query, setQuery] = useState('');
	const [searchIds, setSearchIds] = useState<Array<string>>([]);
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
	const selectedKeys = useMemo(() => new Set(selected.map(overwriteKey)), [selected]);
	const roles = useMemo(() => {
		if (!guild) return [];
		const all = Object.values(guild.roles)
			.filter((role) => role.id !== guildId)
			.sort((a, b) => b.position - a.position);
		const trimmed = query.trim();
		return trimmed ? matchSorter(all, trimmed, {keys: ['name']}) : all;
	}, [guild, guildId, query]);
	const members = selectOverrideMembers({
		cachedMembers: GuildMembers.getMembers(guildId),
		workerMemberIds: searchIds,
		resolveMember: (userId) => GuildMembers.getMember(guildId, userId),
		excludedIds: new Set(),
		guildId,
		query,
		limit: MEMBER_LIMIT,
	});
	const toggle = (entry: ChannelCreateOverwrite, checked: boolean) => {
		const key = overwriteKey(entry);
		onChange(checked ? [...selected, entry] : selected.filter((item) => overwriteKey(item) !== key));
	};
	return (
		<div className={styles.accessStep} data-flx="channel.channel-create-access-step.access-step">
			<div className={styles.accessSearch} data-flx="channel.channel-create-access-step.access-search">
				<Input
					type="text"
					value={query}
					onChange={(event) => setQuery(event.target.value)}
					placeholder={i18n._(SEARCH_MEMBERS_OR_ROLES_DESCRIPTOR)}
					aria-label={i18n._(SEARCH_MEMBERS_OR_ROLES_DESCRIPTOR)}
					autoComplete="off"
					leftIcon={
						<MagnifyingGlassIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="channel.channel-create-access-step.magnifying-glass-icon"
						/>
					}
					data-flx="channel.channel-create-access-step.search-input.text"
				/>
				<p className={styles.accessHint} data-flx="channel.channel-create-access-step.access-hint">
					{i18n._(ACCESS_HINT_DESCRIPTOR)}
				</p>
			</div>
			<div className={styles.accessList} data-flx="channel.channel-create-access-step.access-list">
				{roles.length > 0 && (
					<section className={styles.accessSection} data-flx="channel.channel-create-access-step.roles">
						<h3 className={styles.accessHeading} data-flx="channel.channel-create-access-step.roles-heading">
							{i18n._(ROLES_DESCRIPTOR)}
						</h3>
						{roles.map((role) => {
							const entry: ChannelCreateOverwrite = {id: role.id, type: 0};
							return (
								<div key={role.id} className={styles.accessRow} data-flx="channel.channel-create-access-step.role-row">
									<Checkbox
										checked={selectedKeys.has(overwriteKey(entry))}
										onChange={(checked) => toggle(entry, checked)}
										data-flx="channel.channel-create-access-step.checkbox.role"
									>
										<span className={styles.accessLabel} data-flx="channel.channel-create-access-step.role-label">
											<span
												className={styles.roleDot}
												style={{backgroundColor: role.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(role.color)}}
												data-flx="channel.channel-create-access-step.role-dot"
											/>
											{role.name}
										</span>
									</Checkbox>
								</div>
							);
						})}
					</section>
				)}
				{members.length > 0 && (
					<section className={styles.accessSection} data-flx="channel.channel-create-access-step.members">
						<h3 className={styles.accessHeading} data-flx="channel.channel-create-access-step.members-heading">
							{i18n._(MEMBERS_DESCRIPTOR)}
						</h3>
						{members.map((member) => {
							const entry: ChannelCreateOverwrite = {id: member.user.id, type: 1};
							return (
								<div
									key={member.user.id}
									className={styles.accessRow}
									data-flx="channel.channel-create-access-step.member-row"
								>
									<Checkbox
										checked={selectedKeys.has(overwriteKey(entry))}
										onChange={(checked) => toggle(entry, checked)}
										data-flx="channel.channel-create-access-step.checkbox.member"
									>
										<span className={styles.accessLabel} data-flx="channel.channel-create-access-step.member-label">
											<Avatar
												user={member.user}
												size={24}
												guildId={guildId}
												data-flx="channel.channel-create-access-step.avatar"
											/>
											{getOverrideMemberLabel(member, guildId)}
										</span>
									</Checkbox>
								</div>
							);
						})}
					</section>
				)}
				{roles.length === 0 && members.length === 0 && (
					<p className={styles.accessEmpty} data-flx="channel.channel-create-access-step.access-empty">
						{i18n._(NO_MATCHES_DESCRIPTOR)}
					</p>
				)}
			</div>
		</div>
	);
});
