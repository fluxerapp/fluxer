// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelUserTag.module.css';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import React from 'react';

interface UserTagProps extends React.ComponentPropsWithoutRef<'span'> {
	className?: string;
	system?: boolean;
	variant?: 'bot' | 'system' | 'community' | 'op';
	size?: 'sm' | 'lg';
}

function renderTagLabel(variant: 'bot' | 'system' | 'community' | 'op') {
	switch (variant) {
		case 'op':
			return (
				<Trans comment="Tag next to the author of a forum post on their own messages in that post. Short for original poster, keep it to two or three letters.">
					OP
				</Trans>
			);
		case 'system':
			return <Trans>System</Trans>;
		case 'community':
			return (
				<Trans comment="Tag shown next to messages copied from a followed announcement channel. Keep it one short word.">
					Community
				</Trans>
			);
		default:
			return <Trans>Bot</Trans>;
	}
}

export const UserTag = React.forwardRef<HTMLSpanElement, UserTagProps>(
	({className, system, variant, size = 'sm', ...props}, ref) => {
		return (
			<span
				className={clsx(styles.tag, size === 'lg' ? styles.tagLg : styles.tagSm, className)}
				ref={ref}
				data-flx="channel.user-tag.tag"
				{...props}
			>
				<span
					className={clsx(styles.text, size === 'lg' ? styles.textLg : styles.textSm)}
					data-flx="channel.user-tag.text"
				>
					{renderTagLabel(variant ?? (system ? 'system' : 'bot'))}
				</span>
			</span>
		);
	},
);

UserTag.displayName = 'UserTag';
