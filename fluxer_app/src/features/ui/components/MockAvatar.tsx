// SPDX-License-Identifier: AGPL-3.0-or-later

import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import {BaseAvatar} from '@app/features/ui/components/BaseAvatar';
import {getDefaultAvatarURLForIndex} from '@app/features/user/utils/AvatarUtils';
import {useLingui} from '@lingui/react/macro';
import React from 'react';

interface MockAvatarProps {
	size: 12 | 16 | 20 | 24 | 32 | 36 | 40 | 48 | 56 | 80 | 120;
	avatarUrl?: string;
	hoverAvatarUrl?: string;
	status?: string | null;
	isTyping?: boolean;
	showOffline?: boolean;
	className?: string;
	isClickable?: boolean;
	userTag?: string;
	disableStatusTooltip?: boolean;
	shouldPlayAnimated?: boolean;
	isMobileStatus?: boolean;
}

export const MockAvatar = React.forwardRef<HTMLDivElement, MockAvatarProps>(
	(
		{
			size,
			avatarUrl = getDefaultAvatarURLForIndex(0),
			hoverAvatarUrl,
			status,
			isTyping = false,
			showOffline = true,
			className,
			isClickable = false,
			userTag = 'Mock User',
			disableStatusTooltip = false,
			shouldPlayAnimated = false,
			isMobileStatus = false,
			...props
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const statusLabel = status != null ? getStatusTypeLabel(i18n, status) : null;
		return (
			<BaseAvatar
				ref={ref}
				size={size}
				avatarUrl={avatarUrl}
				hoverAvatarUrl={hoverAvatarUrl}
				status={status}
				shouldPlayAnimated={shouldPlayAnimated}
				isTyping={isTyping}
				showOffline={showOffline}
				className={className}
				isClickable={isClickable}
				userTag={userTag}
				statusLabel={statusLabel}
				disableStatusTooltip={disableStatusTooltip}
				isMobileStatus={isMobileStatus}
				data-flx="ui.mock-avatar.base-avatar"
				{...props}
			/>
		);
	},
);

MockAvatar.displayName = 'MockAvatar';
