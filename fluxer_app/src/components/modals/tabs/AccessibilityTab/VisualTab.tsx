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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {Switch} from '~/components/form/Switch';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {Slider} from '~/components/uikit/Slider';
import AccessibilityStore, {DMMessagePreviewMode} from '~/stores/AccessibilityStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './VisualTab.module.css';

const dmMessagePreviewOptions = (
	t: (msg: MessageDescriptor) => string,
): ReadonlyArray<RadioOption<DMMessagePreviewMode>> => [
	{
		value: DMMessagePreviewMode.ALL,
		name: t(msg`All messages`),
		desc: t(msg`Show message previews for all DM conversations`),
	},
	{
		value: DMMessagePreviewMode.UNREAD_ONLY,
		name: t(msg`Unread DMs only`),
		desc: t(msg`Only show message previews for DMs with unread messages`),
	},
	{
		value: DMMessagePreviewMode.NONE,
		name: t(msg`None`),
		desc: t(msg`Don't show message previews in the DM list`),
	},
];

export const VisualTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const saturationFactor = AccessibilityStore.saturationFactor;
	const alwaysUnderlineLinks = AccessibilityStore.alwaysUnderlineLinks;
	const enableTextSelection = AccessibilityStore.enableTextSelection;
	const mobileLayout = MobileLayoutStore;

	return (
		<>
			<div className={styles.sliderSection}>
				<div className={styles.sliderHeader}>
					<label htmlFor="saturation" className={styles.sliderLabel}>
						<Trans>Saturation</Trans>
					</label>
					<p className={styles.sliderDescription}>
						<Trans>Adjust the saturation of all theme colors.</Trans>
					</p>
				</div>
				<Slider
					defaultValue={saturationFactor * 100}
					factoryDefaultValue={100}
					minValue={0}
					maxValue={100}
					step={1}
					markers={[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]}
					stickToMarkers={false}
					onMarkerRender={(value) => `${value}%`}
					onValueRender={(value) => <Trans>{value}%</Trans>}
					onValueChange={(value) => AccessibilityActionCreators.update({saturationFactor: value / 100})}
				/>
			</div>

			<Switch
				label={t(msg`Always underline links`)}
				description={t(msg`Make links to websites stand out more by always underlining them.`)}
				value={alwaysUnderlineLinks}
				onChange={(value) => AccessibilityActionCreators.update({alwaysUnderlineLinks: value})}
			/>

			<SettingsTabSection
				title={<Trans>Interaction</Trans>}
				description={<Trans>Adjust how you interact with the app</Trans>}
			>
				<Switch
					label={t(msg`Enable text selection`)}
					description={
						mobileLayout.enabled
							? t(
									msg`Allow selecting all text content in the app. This setting is disabled on mobile to prevent interference with touch interactions.`,
								)
							: t(msg`Allow selecting all text content in the app.`)
					}
					value={enableTextSelection}
					disabled={mobileLayout.enabled}
					onChange={(value) => AccessibilityActionCreators.update({enableTextSelection: value})}
				/>
			</SettingsTabSection>

			<SettingsTabSection
				title={<Trans>DM Message Previews</Trans>}
				description={<Trans>Control when message previews are shown in the DM list</Trans>}
			>
				<RadioGroup
					options={dmMessagePreviewOptions(t)}
					value={AccessibilityStore.dmMessagePreviewMode}
					onChange={(value) => AccessibilityActionCreators.update({dmMessagePreviewMode: value})}
					aria-label={t(msg`DM message preview mode`)}
				/>
			</SettingsTabSection>
		</>
	);
});
