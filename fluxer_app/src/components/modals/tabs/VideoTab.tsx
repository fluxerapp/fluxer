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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as VoiceSettingsActionCreators from '~/actions/VoiceSettingsActionCreators';
import {Select} from '~/components/form/Select';
import {CameraPreviewModalStandalone} from '~/components/modals/CameraPreviewModal';
import styles from '~/components/modals/tabs/VideoTab.module.css';
import {Button} from '~/components/uikit/Button/Button';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {Slider} from '~/components/uikit/Slider';
import type VoiceSettingsStore from '~/stores/VoiceSettingsStore';
import {useMediaPermission} from './hooks/useMediaPermission';

interface VideoTabProps {
	voiceSettings: typeof VoiceSettingsStore;
	hasPremium: boolean;
	autoRequestPermission?: boolean;
}

export const VideoTab: React.FC<VideoTabProps> = observer(
	({voiceSettings, hasPremium, autoRequestPermission = true}) => {
		const {t} = useLingui();
		const {videoDeviceId, cameraResolution, screenshareResolution, videoFrameRate} = voiceSettings;

		const {
			devices,
			status: permissionStatus,
			requestPermission,
		} = useMediaPermission('video', {
			autoRequest: autoRequestPermission,
		});

		React.useEffect(() => {
			if (videoDeviceId === 'default' && devices.length > 0) {
				VoiceSettingsActionCreators.update({videoDeviceId: devices[0].deviceId});
			}
		}, [devices, videoDeviceId]);

		const videoDeviceOptions =
			devices.length > 0
				? devices.map((device) => {
						const shortDeviceId = device.deviceId.slice(0, 8);
						return {
							value: device.deviceId,
							label: device.label || t`Camera ${shortDeviceId}`,
						};
					})
				: [{value: 'default', label: t`Default`}];

		const effectiveVideoDeviceId = devices.length === 0 ? 'default' : videoDeviceId;

		const cameraResolutionOptions: ReadonlyArray<RadioOption<'low' | 'medium' | 'high'>> = [
			{value: 'low', name: t`Low (480p)`, desc: t`Best for slower connections`},
			{value: 'medium', name: t`Medium (720p)`, desc: t`Balanced quality and bandwidth`},
			{value: 'high', name: t`High (1080p)`, desc: t`Best quality for camera`},
		];

		const screenshareResolutionOptions: ReadonlyArray<RadioOption<'low' | 'medium' | 'high' | 'ultra' | '4k'>> = [
			{value: 'low', name: t`Low (480p)`, desc: t`Best for slower connections`},
			{value: 'medium', name: t`Medium (720p)`, desc: t`Balanced quality and bandwidth`},
			{
				value: 'high',
				name: t`High (1080p)`,
				desc: hasPremium ? t`Best quality for most users` : t`Requires Plutonium`,
				disabled: !hasPremium,
			},
			{
				value: 'ultra',
				name: t`Ultra (1440p)`,
				desc: hasPremium ? t`High quality, requires fast connection` : t`Requires Plutonium`,
				disabled: !hasPremium,
			},
			{
				value: '4k',
				name: t`4K (2160p)`,
				desc: hasPremium ? t`Maximum quality, requires very fast connection` : t`Requires Plutonium`,
				disabled: !hasPremium,
			},
		];

		const handleCameraPreview = async () => {
			const granted = await requestPermission();
			if (granted) {
				ModalActionCreators.push(modal(() => <CameraPreviewModalStandalone showEnableCameraButton={false} />));
			}
		};

		return (
			<>
				{devices.length === 0 && permissionStatus !== 'loading' && permissionStatus !== 'granted' ? (
					<div className={styles.deviceNotice}>
						<div className={styles.deviceNoticeText}>
							<div className={styles.deviceNoticeTitle}>
								<Trans>No cameras detected</Trans>
							</div>
							<p className={styles.deviceNoticeDescription}>
								{permissionStatus === 'denied' ? (
									<Trans>
										Allow Fluxer to access your camera in System Settings → Privacy &amp; Security → Camera to preview
										and select devices.
									</Trans>
								) : (
									<Trans>Fluxer needs access to your camera before we can list it here.</Trans>
								)}
							</p>
						</div>
						<Button
							variant="secondary"
							small={true}
							onClick={() => {
								void requestPermission();
							}}
						>
							<Trans>Allow Camera</Trans>
						</Button>
					</div>
				) : null}

				<div>
					<Select
						label={t`Camera`}
						value={effectiveVideoDeviceId}
						options={videoDeviceOptions}
						onChange={(value) => VoiceSettingsActionCreators.update({videoDeviceId: value})}
					/>
				</div>

				<Button variant="primary" fitContainer={false} className={styles.actionButton} onClick={handleCameraPreview}>
					<Trans>Start Camera Test &amp; Configure Effects</Trans>
				</Button>

				<div>
					<div className={styles.sectionTitle}>
						<Trans>Camera Quality</Trans>
					</div>
					<p className={styles.sectionDescription}>
						<Trans>Maximum quality for camera is 1080p</Trans>
					</p>
					<RadioGroup
						aria-label={t`Camera quality`}
						options={cameraResolutionOptions}
						value={cameraResolution}
						onChange={(value) => VoiceSettingsActionCreators.update({cameraResolution: value})}
					/>
				</div>

				<div>
					<div className={styles.sectionTitle}>
						<Trans>Screen Sharing Quality</Trans>
					</div>
					<RadioGroup
						aria-label={t`Screen sharing quality`}
						options={screenshareResolutionOptions}
						value={screenshareResolution}
						onChange={(value) => VoiceSettingsActionCreators.update({screenshareResolution: value})}
					/>
				</div>

				{!hasPremium && (
					<div className={styles.premiumCard}>
						<div className={styles.premiumHeader}>
							<CrownIcon weight="fill" size={18} className={styles.premiumIcon} />
							<span className={styles.premiumTitle}>
								<Trans>Unlock HD Screen Sharing with Plutonium</Trans>
							</span>
						</div>
						<p className={styles.premiumDescription}>
							<Trans>
								Get crystal clear screen sharing with High (1080p), Ultra (1440p), and 4K (2160p) resolutions, plus
								unlock frame rates up to 60 fps for the smoothest experience.
							</Trans>
						</p>
						<Button variant="secondary" small={true} onClick={() => PremiumModalActionCreators.open()}>
							<Trans>Get Plutonium</Trans>
						</Button>
					</div>
				)}

				<div>
					<div className={styles.sectionTitle}>
						<Trans>Frame Rate</Trans>
					</div>
					<p className={styles.sectionDescription}>
						<Trans>Applies to both camera and screen sharing</Trans>
					</p>
					<Slider
						defaultValue={videoFrameRate}
						factoryDefaultValue={30}
						minValue={15}
						maxValue={hasPremium ? 60 : 30}
						markers={hasPremium ? [15, 24, 30, 60] : [15, 24, 30]}
						stickToMarkers={true}
						onMarkerRender={(value) => `${Math.round(value)}fps`}
						onValueChange={(value) => VoiceSettingsActionCreators.update({videoFrameRate: value})}
					/>
					{!hasPremium && (
						<div className={styles.frameRateNote}>
							<CrownIcon weight="fill" size={14} className={styles.frameRateIcon} />
							<Trans>Frame rates above 30 fps require Plutonium</Trans>
						</div>
					)}
				</div>
			</>
		);
	},
);
