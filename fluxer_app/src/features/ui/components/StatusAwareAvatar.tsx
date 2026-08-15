// SPDX-License-Identifier: AGPL-3.0-or-later

import Presence from '@app/features/presence/state/Presence';
import TransientPresence from '@app/features/presence/state/TransientPresence';
import {Avatar} from '@app/features/ui/components/Avatar';
import type {User} from '@app/features/user/models/User';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {observer} from 'mobx-react-lite';
import type React from 'react';

export interface StatusAwareAvatarProps {
	user: User | null;
	size: number;
	forceAnimate?: boolean;
	forceAnimateIgnoringSettings?: boolean;
	isTyping?: boolean;
	showOffline?: boolean;
	className?: string;
	isClickable?: boolean;
	disablePresence?: boolean;
	disableStatusTooltip?: boolean;
	avatarUrl?: string | null;
	hoverAvatarUrl?: string | null;
	guildId?: string | null;
	mediaSize?: MediaProxyImageSize;
	deferImageLoad?: boolean;
	status?: string | null;
	animateStatusCutout?: boolean;
}

function getStatusWithTransientFallback(userId: string): StatusType {
	const presenceStatus = Presence.getStatus(userId);
	if (presenceStatus !== StatusTypes.OFFLINE) {
		return presenceStatus;
	}
	return TransientPresence.getStatus(userId);
}

export const StatusAwareAvatar: React.FC<StatusAwareAvatarProps> = observer(
	({
		user,
		size,
		forceAnimate,
		forceAnimateIgnoringSettings,
		isTyping,
		showOffline,
		className,
		isClickable,
		disablePresence,
		disableStatusTooltip = false,
		avatarUrl,
		hoverAvatarUrl,
		guildId,
		mediaSize,
		deferImageLoad,
		status: externalStatus,
		animateStatusCutout,
	}) => {
		if (!user) {
			return null;
		}
		const shouldDisablePresence = disablePresence || user.system;
		let status = externalStatus;
		if (shouldDisablePresence) {
			status = null;
		} else if (externalStatus == null) {
			status = getStatusWithTransientFallback(user.id);
		}
		const isMobile = shouldDisablePresence ? false : Presence.isMobile(user.id);
		return (
			<Avatar
				user={user}
				size={size}
				status={status}
				isMobileStatus={isMobile}
				forceAnimate={forceAnimate}
				forceAnimateIgnoringSettings={forceAnimateIgnoringSettings}
				isTyping={isTyping}
				showOffline={showOffline}
				className={className}
				isClickable={isClickable}
				disableStatusTooltip={disableStatusTooltip}
				avatarUrl={avatarUrl}
				hoverAvatarUrl={hoverAvatarUrl}
				guildId={guildId}
				mediaSize={mediaSize}
				deferImageLoad={deferImageLoad}
				animateStatusCutout={animateStatusCutout}
				data-flx="ui.status-aware-avatar.avatar"
			/>
		);
	},
);

export type ListStatusAwareAvatarProps = Omit<StatusAwareAvatarProps, 'animateStatusCutout'>;

export const ListStatusAwareAvatar: React.FC<ListStatusAwareAvatarProps> = (props) => (
	<StatusAwareAvatar
		data-flx="ui.status-aware-avatar.list-status-aware-avatar.status-aware-avatar"
		{...props}
		animateStatusCutout
	/>
);
