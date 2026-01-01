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

import {Trans, useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';
import {Switch} from '~/components/form/Switch';
import * as Modal from '~/components/modals/Modal';
import styles from '~/components/modals/ScreenShareSettingsModal.module.css';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {useScreenShareSettingsModal} from '~/utils/modals/ScreenShareSettingsModalUtils';

interface ScreenShareSettingsModalProps {
	onStartShare: (
		resolution: 'low' | 'medium' | 'high' | 'ultra' | '4k',
		frameRate: number,
		includeAudio: boolean,
	) => Promise<void>;
}

export const ScreenShareSettingsModal = observer(({onStartShare}: ScreenShareSettingsModalProps) => {
	const {t} = useLingui();
	const {
		hasPremium,
		isSharing,
		selectedResolution,
		selectedFrameRate,
		includeAudio,
		setIncludeAudio,
		handleStartShare,
		handleCancel,
		handleResolutionClick,
		handleFrameRateClick,
		RESOLUTION_OPTIONS,
		FRAMERATE_OPTIONS,
	} = useScreenShareSettingsModal({onStartShare});

	const resolutionOptions = useMemo(
		() => RESOLUTION_OPTIONS.map((option) => ({...option, label: t(option.label)})),
		[t],
	);

	const framerateOptions = useMemo(() => FRAMERATE_OPTIONS.map((option) => ({...option, label: t(option.label)})), [t]);

	const getOptionButtonClass = (isSelected: boolean, isLocked: boolean) =>
		clsx(styles.optionButton, {
			[styles.optionButtonSelected]: isSelected && !isLocked,
			[styles.optionButtonSelectedLocked]: isSelected && isLocked,
			[styles.optionButtonUnselected]: !isSelected && !isLocked,
			[styles.optionButtonUnselectedLocked]: !isSelected && isLocked,
		});

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Screen Share Settings`} />
			<Modal.Content>
				<div className={styles.content}>
					<div className={styles.section}>
						<div className={styles.sectionLabel}>
							<Trans>Video Quality</Trans>
						</div>
						<div className={styles.optionGrid}>
							{resolutionOptions.map((option) => {
								const isSelected = selectedResolution === option.value;
								const isLocked = option.isPremium && !hasPremium;

								return (
									<FocusRing key={option.value} offset={-2}>
										<button
											type="button"
											onClick={() => handleResolutionClick(option.value, option.isPremium)}
											className={getOptionButtonClass(isSelected, isLocked)}
										>
											{isLocked && <CrownIcon weight="fill" size={14} className={styles.lockIcon} />}
											{option.label}
										</button>
									</FocusRing>
								);
							})}
						</div>
					</div>

					<div className={styles.section}>
						<div className={styles.sectionLabel}>
							<Trans>Frame Rate</Trans>
						</div>
						<div className={styles.optionGrid}>
							{framerateOptions.map((option) => {
								const isSelected = selectedFrameRate === option.value;
								const isLocked = option.isPremium && !hasPremium;

								return (
									<FocusRing key={option.value} offset={-2}>
										<button
											type="button"
											onClick={() => handleFrameRateClick(option.value, option.isPremium)}
											className={getOptionButtonClass(isSelected, isLocked)}
										>
											{isLocked && <CrownIcon weight="fill" size={14} className={styles.lockIcon} />}
											{option.label}
										</button>
									</FocusRing>
								);
							})}
						</div>
					</div>

					<div className={styles.section}>
						<div className={styles.audioToggleRow}>
							<div className={styles.audioToggleInfo}>
								<div className={styles.sectionLabel}>
									<Trans>Share Audio</Trans>
								</div>
								<div className={styles.audioToggleDescription}>
									<Trans>Include audio from your screen in the share</Trans>
								</div>
							</div>
							<Switch value={includeAudio} onChange={setIncludeAudio} />
						</div>
					</div>

					{!hasPremium && (
						<div className={styles.premiumBanner}>
							<div className={styles.premiumBannerHeader}>
								<CrownIcon weight="fill" size={16} className={styles.premiumBannerIcon} />
								<span className={styles.premiumBannerTitle}>
									<Trans>Unlock HD Video with Plutonium</Trans>
								</span>
							</div>
							<p className={styles.premiumBannerDescription}>
								<Trans>
									Get High (1080p), Ultra (1440p), and 4K (2160p) resolutions, plus 60 fps for the smoothest experience.
								</Trans>
							</p>
						</div>
					)}
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={handleCancel}>
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleStartShare} submitting={isSharing}>
					<Trans>Start Sharing</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
