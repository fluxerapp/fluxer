// SPDX-License-Identifier: AGPL-3.0-or-later

import {SkeletonBlock} from '@app/features/app/components/skeleton/SkeletonBlock';
import {SkeletonCircle} from '@app/features/app/components/skeleton/SkeletonCircle';
import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {SkeletonEmphasis, SkeletonRadius} from '@app/features/app/components/skeleton/SkeletonStyle';
import styles from '@app/features/app/components/skeleton/UserAreaSkeleton.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {flxElementClassName} from '@app/lib/react';
import type React from 'react';

const AVATAR_SIZE = remFromPx(32);
const NAME_LINE_WIDTH = '6.5rem';
const NAME_LINE_HEIGHT = '0.75rem';
const STATUS_LINE_WIDTH = '4.25rem';
const STATUS_LINE_HEIGHT = '0.5rem';
const CONTROL_BUTTON_SIZE = '2rem';

interface UserAreaSkeletonProps {
	readonly voicePanelHeightPx: number;
}

export const UserAreaSkeleton: React.FC<UserAreaSkeletonProps> = ({voicePanelHeightPx}) => (
	<flx-app-user-area-skeleton
		className={flxElementClassName(styles.userArea)}
		aria-hidden
		data-flx="app.skeleton.user-area-skeleton.user-area"
	>
		<flx-app-user-area-skeleton-panel
			className={flxElementClassName(styles.panel)}
			data-flx="app.skeleton.user-area-skeleton.panel"
		>
			{voicePanelHeightPx > 0 && (
				<flx-app-user-area-skeleton-voice
					className={flxElementClassName(styles.voicePanel)}
					style={{height: `${voicePanelHeightPx}px`}}
					data-flx="app.skeleton.user-area-skeleton.voice-panel"
				/>
			)}
			<flx-app-user-area-skeleton-content
				className={flxElementClassName(styles.content)}
				data-flx="app.skeleton.user-area-skeleton.content"
			>
				<flx-app-user-area-skeleton-identity
					className={flxElementClassName(styles.identity)}
					data-flx="app.skeleton.user-area-skeleton.identity"
				>
					<SkeletonCircle
						size={AVATAR_SIZE}
						emphasis={SkeletonEmphasis.STRONG}
						data-flx="app.skeleton.user-area-skeleton.skeleton-circle"
					/>
					<flx-app-user-area-skeleton-identity-text
						className={flxElementClassName(styles.identityText)}
						data-flx="app.skeleton.user-area-skeleton.identity-text"
					>
						<flx-app-user-area-skeleton-name
							className={flxElementClassName(styles.nameRow)}
							data-flx="app.skeleton.user-area-skeleton.name-row"
						>
							<SkeletonLine
								width={NAME_LINE_WIDTH}
								height={NAME_LINE_HEIGHT}
								data-flx="app.skeleton.user-area-skeleton.skeleton-line"
							/>
						</flx-app-user-area-skeleton-name>
						<flx-app-user-area-skeleton-status
							className={flxElementClassName(styles.statusRow)}
							data-flx="app.skeleton.user-area-skeleton.status-row"
						>
							<SkeletonLine
								width={STATUS_LINE_WIDTH}
								height={STATUS_LINE_HEIGHT}
								emphasis={SkeletonEmphasis.MUTED}
								data-flx="app.skeleton.user-area-skeleton.skeleton-line--2"
							/>
						</flx-app-user-area-skeleton-status>
					</flx-app-user-area-skeleton-identity-text>
				</flx-app-user-area-skeleton-identity>
				<flx-app-user-area-skeleton-controls
					className={flxElementClassName(styles.controls)}
					data-flx="app.skeleton.user-area-skeleton.controls"
				>
					<SkeletonBlock
						width={CONTROL_BUTTON_SIZE}
						height={CONTROL_BUTTON_SIZE}
						radius={SkeletonRadius.MEDIUM}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.user-area-skeleton.skeleton-block"
					/>
					<SkeletonBlock
						width={CONTROL_BUTTON_SIZE}
						height={CONTROL_BUTTON_SIZE}
						radius={SkeletonRadius.MEDIUM}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.user-area-skeleton.skeleton-block--2"
					/>
					<SkeletonBlock
						width={CONTROL_BUTTON_SIZE}
						height={CONTROL_BUTTON_SIZE}
						radius={SkeletonRadius.MEDIUM}
						emphasis={SkeletonEmphasis.MUTED}
						data-flx="app.skeleton.user-area-skeleton.skeleton-block--3"
					/>
				</flx-app-user-area-skeleton-controls>
			</flx-app-user-area-skeleton-content>
		</flx-app-user-area-skeleton-panel>
	</flx-app-user-area-skeleton>
);
