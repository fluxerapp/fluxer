/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon} from '@phosphor-icons/react';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useHover} from '~/hooks/useHover';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {openExternalUrl} from '~/utils/NativeUtils';
import guildStyles from '../GuildsLayout.module.css';
import styles from './DownloadButton.module.css';

export const DownloadButton = observer(() => {
	const {t} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = React.useRef<HTMLButtonElement | null>(null);
	const iconRef = React.useRef<HTMLDivElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef]);

	const handleDownload = () => {
		openExternalUrl('https://fluxer.app/download');
	};

	return (
		<div className={guildStyles.addGuildButton}>
			<Tooltip position="right" size="large" text={() => t`Download Fluxer`}>
				<FocusRing offset={-2} focusTarget={buttonRef} ringTarget={iconRef}>
					<button
						type="button"
						aria-label={t`Download Fluxer`}
						onClick={handleDownload}
						className={styles.button}
						ref={mergedButtonRef}
					>
						<motion.div
							ref={iconRef}
							className={guildStyles.addGuildButtonIcon}
							animate={{borderRadius: isHovering ? '30%' : '50%'}}
							initial={{borderRadius: isHovering ? '30%' : '50%'}}
							transition={{duration: 0.07, ease: 'easeOut'}}
							whileHover={{borderRadius: '30%'}}
						>
							<DownloadSimpleIcon weight="bold" className={styles.iconText} />
						</motion.div>
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
