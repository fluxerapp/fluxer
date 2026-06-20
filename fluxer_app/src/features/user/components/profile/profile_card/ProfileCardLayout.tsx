// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/user/components/profile/profile_card/ProfileCardLayout.module.css';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

interface ProfileCardLayoutProps {
	borderColor: string;
	borderGradient: string;
	showPreviewLabel?: boolean;
	hoverRef?: (instance: HTMLDivElement | null) => void;
	className?: string;
	style?: React.CSSProperties;
	children: React.ReactNode;
}

export const ProfileCardLayout: React.FC<ProfileCardLayoutProps> = observer(
	({borderColor, borderGradient, showPreviewLabel = false, hoverRef, className, style, children}) => {
		const frameStyle = useMemo<React.CSSProperties>(
			() => ({
				...style,
				['--profile-card-border-color' as string]: borderColor,
				['--profile-card-border-gradient' as string]: borderGradient,
			}),
			[borderColor, borderGradient, style],
		);
		return (
			<div data-flx="user.profile.profile-card.profile-card-layout.div">
				{showPreviewLabel && (
					<div className={styles.previewLabel} data-flx="user.profile.profile-card.profile-card-layout.preview-label">
						<Trans>Profile preview</Trans>
					</div>
				)}
				<div
					className={clsx(styles.profileCardFrame, className)}
					style={frameStyle}
					data-flx="user.profile.profile-card.profile-card-layout.profile-card-frame"
				>
					<div
						ref={hoverRef}
						className={styles.profileCardInner}
						data-flx="user.profile.profile-card.profile-card-layout.profile-card"
					>
						{children}
					</div>
				</div>
			</div>
		);
	},
);
