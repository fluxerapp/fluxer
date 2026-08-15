// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteItem} from '@app/features/channel/components/AutocompleteItem';
import styles from '@app/features/channel/components/AutocompleteMention.module.css';
import {
	type AutocompleteMentionMemberOption,
	type AutocompleteMentionRoleOption,
	type AutocompleteMentionUserOption,
	type AutocompleteOption,
	type AutocompleteSpecialMentionOption,
	isMentionMember,
	isMentionRole,
	isMentionUser,
	isSpecialMention,
} from '@app/features/channel/components/AutocompleteTypes';
import Guilds from '@app/features/guild/state/Guilds';
import {useParams} from '@app/features/platform/components/router/RouterReact';
import * as ColorUtils from '@app/features/theme/utils/ColorUtils';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import * as DisplayNameUtils from '@app/features/user/utils/DisplayNameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const NOTIFY_EVERYONE_WHO_HAS_PERMISSION_TO_VIEW_THIS_DESCRIPTOR = msg({
	message: 'Notify everyone who has permission to view this channel.',
	comment: 'Description text in the channel and chat autocomplete mention. Keep the tone plain and specific.',
});
const NOTIFY_EVERYONE_ONLINE_WHO_HAS_PERMISSION_TO_VIEW_DESCRIPTOR = msg({
	message: 'Notify everyone online who has permission to view this channel.',
	comment: 'Description text in the channel and chat autocomplete mention. Keep the tone plain and specific.',
});
const NOTIFY_USERS_WITH_THIS_ROLE_WHO_HAVE_PERMISSION_DESCRIPTOR = msg({
	message: 'Notify users with this role who have permission to view this channel.',
	comment: 'Description text in the channel and chat autocomplete mention. Keep the tone plain and specific.',
});

type RowRef = React.MutableRefObject<Array<HTMLButtonElement | null>> | undefined;
type OptionIdGetter = ((index: number) => string) | undefined;

function resolveOptionId(getOptionId: OptionIdGetter, index: number): string | undefined {
	if (getOptionId == null) {
		return undefined;
	}
	return getOptionId(index);
}

function resolveRowRef(rowRefs: RowRef, index: number): React.Ref<HTMLButtonElement> | undefined {
	if (rowRefs == null) {
		return undefined;
	}
	return (node: HTMLButtonElement | null) => {
		rowRefs.current[index] = node;
	};
}

function renderDivider(): React.ReactNode {
	return <div className={styles.divider} aria-hidden={true} data-flx="channel.autocomplete-mention.divider" />;
}

export const AutocompleteMention = observer(function AutocompleteMention({
	onSelect,
	keyboardFocusIndex,
	hoverIndex,
	options,
	onMouseEnter,
	onMouseLeave,
	rowRefs,
	getOptionId,
}: {
	onSelect: (option: AutocompleteOption) => void;
	keyboardFocusIndex: number;
	hoverIndex: number;
	options: Array<AutocompleteOption>;
	onMouseEnter: (index: number) => void;
	onMouseLeave: () => void;
	rowRefs?: React.MutableRefObject<Array<HTMLButtonElement | null>>;
	getOptionId?: (index: number) => string;
}) {
	const {i18n} = useLingui();
	const routeParams = useParams() as {guildId?: string};
	const guildId = routeParams.guildId;
	const guild = Guilds.getGuild(guildId == null ? '' : guildId);
	const nicknameGuildId = guild == null ? undefined : guild.id;
	const members = options.filter(isMentionMember);
	const users = options.filter(isMentionUser);
	const roles = options.filter(isMentionRole);
	const specialMentions = options.filter(isSpecialMention);
	const renderMember = (option: AutocompleteMentionMemberOption, index: number) => (
		<AutocompleteItem
			key={option.member.user.id}
			id={resolveOptionId(getOptionId, index)}
			icon={
				<StatusAwareAvatar
					user={option.member.user}
					size={24}
					guildId={guildId}
					data-flx="channel.autocomplete-mention.status-aware-avatar"
				/>
			}
			name={DisplayNameUtils.getGuildMemberNickname(option.member)}
			description={DisplayNameUtils.formatUserTagForStreamerMode(option.member.user)}
			isKeyboardSelected={index === keyboardFocusIndex}
			isHovered={index === hoverIndex}
			onSelect={() => onSelect(option)}
			onMouseEnter={() => onMouseEnter(index)}
			onMouseLeave={onMouseLeave}
			innerRef={resolveRowRef(rowRefs, index)}
			data-flx="channel.autocomplete-mention.autocomplete-item.select"
		/>
	);
	const renderUser = (option: AutocompleteMentionUserOption, index: number) => {
		const currentIndex = members.length + index;
		return (
			<AutocompleteItem
				key={option.user.id}
				id={resolveOptionId(getOptionId, currentIndex)}
				icon={
					<StatusAwareAvatar
						user={option.user}
						size={24}
						data-flx="channel.autocomplete-mention.status-aware-avatar--2"
					/>
				}
				name={DisplayNameUtils.getNickname(option.user, nicknameGuildId)}
				description={DisplayNameUtils.formatUserTagForStreamerMode(option.user)}
				isKeyboardSelected={currentIndex === keyboardFocusIndex}
				isHovered={currentIndex === hoverIndex}
				onSelect={() => onSelect(option)}
				onMouseEnter={() => onMouseEnter(currentIndex)}
				onMouseLeave={onMouseLeave}
				innerRef={resolveRowRef(rowRefs, currentIndex)}
				data-flx="channel.autocomplete-mention.autocomplete-item.select--2"
			/>
		);
	};
	const renderSpecialMention = (option: AutocompleteSpecialMentionOption, index: number) => {
		const currentIndex = members.length + users.length + index;
		return (
			<AutocompleteItem
				key={option.kind}
				id={resolveOptionId(getOptionId, currentIndex)}
				name={option.kind}
				description={
					option.kind === '@everyone'
						? i18n._(NOTIFY_EVERYONE_WHO_HAS_PERMISSION_TO_VIEW_THIS_DESCRIPTOR)
						: i18n._(NOTIFY_EVERYONE_ONLINE_WHO_HAS_PERMISSION_TO_VIEW_DESCRIPTOR)
				}
				isKeyboardSelected={currentIndex === keyboardFocusIndex}
				isHovered={currentIndex === hoverIndex}
				onSelect={() => onSelect(option)}
				onMouseEnter={() => onMouseEnter(currentIndex)}
				onMouseLeave={onMouseLeave}
				innerRef={resolveRowRef(rowRefs, currentIndex)}
				data-flx="channel.autocomplete-mention.autocomplete-item.select--3"
			/>
		);
	};
	const renderRole = (option: AutocompleteMentionRoleOption, index: number) => {
		const currentIndex = members.length + users.length + specialMentions.length + index;
		const roleColor =
			option.role.color === 0 || Number.isNaN(option.role.color) ? undefined : ColorUtils.int2rgb(option.role.color);
		return (
			<AutocompleteItem
				key={option.role.id}
				id={resolveOptionId(getOptionId, currentIndex)}
				name={<span style={{color: roleColor}}>@{option.role.name}</span>}
				description={i18n._(NOTIFY_USERS_WITH_THIS_ROLE_WHO_HAVE_PERMISSION_DESCRIPTOR)}
				isKeyboardSelected={currentIndex === keyboardFocusIndex}
				isHovered={currentIndex === hoverIndex}
				onSelect={() => onSelect(option)}
				onMouseEnter={() => onMouseEnter(currentIndex)}
				onMouseLeave={onMouseLeave}
				onContextMenu={(event) => openRoleContextMenu(event, option.role.id)}
				innerRef={resolveRowRef(rowRefs, currentIndex)}
				data-flx="channel.autocomplete-mention.autocomplete-item.select--4"
			/>
		);
	};
	let hasRowsAfterMembers = users.length > 0;
	if (!hasRowsAfterMembers) {
		hasRowsAfterMembers = specialMentions.length > 0;
	}
	if (!hasRowsAfterMembers) {
		hasRowsAfterMembers = roles.length > 0;
	}
	return (
		<>
			{members.length > 0 && (
				<>
					{members.map(renderMember)}
					{hasRowsAfterMembers && renderDivider()}
				</>
			)}
			{users.length > 0 && (
				<>
					{users.map(renderUser)}
					{(specialMentions.length > 0 || roles.length > 0) && renderDivider()}
				</>
			)}
			{specialMentions.length > 0 && (
				<>
					{specialMentions.map(renderSpecialMention)}
					{roles.length > 0 && renderDivider()}
				</>
			)}
			{roles.map(renderRole)}
		</>
	);
});
