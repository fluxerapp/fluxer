// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import styles from '@app/features/ui/avatars/AvatarWithPresence.module.css';
import {BaseAvatar} from '@app/features/ui/components/BaseAvatar';
import type {User} from '@app/features/user/models/User';
import {parseAvatarHash} from '@app/features/user/utils/AvatarMediaUtils';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MicrophoneSlashIcon, SpeakerSlashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

const MUTED_VOICE_BADGE_LABEL = msg({
	message: 'Muted',
	comment: 'Accessible label for the muted voice badge shown on an avatar.',
});
const DEAFENED_VOICE_BADGE_LABEL = msg({
	message: 'Deafened',
	comment: 'Accessible label for the deafened voice badge shown on an avatar.',
});

interface Props {
	user: User;
	size: number;
	speaking?: boolean;
	muted?: boolean;
	deafened?: boolean;
	className?: string;
	ariaLabel?: string;
	borderClassName?: string;
	guildId?: string | null;
	title?: never;
}

export const AvatarWithPresence: React.FC<Props> = observer(function AvatarWithPresence({
	user,
	size,
	speaking,
	muted,
	deafened,
	className,
	ariaLabel,
	borderClassName,
	guildId,
}) {
	const {i18n} = useLingui();
	const guildMember = GuildMembers.getMember(guildId || '', user.id);
	const resolveSrc = (animated: boolean) =>
		guildId && guildMember
			? AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId,
					user: {id: user.id, avatar: user.avatar},
					memberAvatar: guildMember.avatar,
					avatarUnset: guildMember.isAvatarUnset(),
					animated,
				})
			: AvatarUtils.getUserAvatarURL({id: user.id, avatar: user.avatar}, animated);
	const displayAvatarHash =
		guildId && guildMember ? (guildMember.isAvatarUnset() ? null : (guildMember.avatar ?? user.avatar)) : user.avatar;
	const isAnimatableAvatar = displayAvatarHash != null && parseAvatarHash(displayAvatarHash).animated;
	const src = resolveSrc(false);
	const animatedSrc = isAnimatableAvatar ? resolveSrc(true) : null;
	const hoverSrc = animatedSrc && animatedSrc !== src ? animatedSrc : undefined;
	const speakingNow = speaking === true;
	const settingsAllowAnimation = useShouldAnimate({
		kind: 'avatar',
		isAnimated: isAnimatableAvatar,
		isHovering: speakingNow && !Accessibility.useReducedMotion,
	});
	const wantsAnimatedAvatar = hoverSrc != null && speakingNow && settingsAllowAnimation;
	const [loadedAnimatedSrc, setLoadedAnimatedSrc] = useState<string | null>(null);
	useEffect(() => {
		if (!hoverSrc || !wantsAnimatedAvatar) return;
		let active = true;
		const cancelAnimatedAvatarLoad = ImageCacheUtils.loadImage(hoverSrc, () => {
			if (active) setLoadedAnimatedSrc(hoverSrc);
		});
		return () => {
			active = false;
			cancelAnimatedAvatarLoad();
		};
	}, [hoverSrc, wantsAnimatedAvatar]);
	const isAnimatedAvatarLoaded = hoverSrc != null && loadedAnimatedSrc === hoverSrc;
	const voiceBadge = deafened ? (
		<SpeakerSlashIcon
			weight="fill"
			aria-hidden
			className={styles.voiceIndicatorIcon}
			data-flx="ui.avatars.avatar-with-presence.voice-indicator-icon"
		/>
	) : muted ? (
		<MicrophoneSlashIcon
			weight="fill"
			aria-hidden
			className={styles.voiceIndicatorIcon}
			data-flx="ui.avatars.avatar-with-presence.voice-indicator-icon--2"
		/>
	) : null;
	const voiceBadgeLabel = deafened
		? i18n._(DEAFENED_VOICE_BADGE_LABEL)
		: muted
			? i18n._(MUTED_VOICE_BADGE_LABEL)
			: null;
	return (
		<BaseAvatar
			size={size}
			avatarUrl={src}
			hoverAvatarUrl={isAnimatedAvatarLoaded ? hoverSrc : undefined}
			shouldPlayAnimated={wantsAnimatedAvatar && isAnimatedAvatarLoaded}
			className={clsx(styles.container, speaking && styles.containerSpeaking, borderClassName, className)}
			userTag={ariaLabel ?? user.displayName}
			disableStatusTooltip
			customStatusBadge={voiceBadge}
			customStatusBadgeColor="var(--status-danger)"
			customStatusBadgeLabel={voiceBadgeLabel}
			customStatusBadgeMaskId="flx-mask-presence-online"
			customStatusBadgeScale={1.5}
			customStatusBadgeMaxSizeRatio={0.36}
			customStatusBadgeCutoutPaddingScale={1.35}
			data-flx="ui.avatars.avatar-with-presence.container"
		/>
	);
});
