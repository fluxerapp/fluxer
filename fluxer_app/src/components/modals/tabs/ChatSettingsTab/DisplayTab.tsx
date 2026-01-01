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
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {RenderSpoilers} from '~/Constants';
import {Switch} from '~/components/form/Switch';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {SwitchGroup, SwitchGroupItem} from '~/components/uikit/SwitchGroup';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from './DisplayTab.module.css';

const spoilerOptions = (t: (m: MessageDescriptor) => string): ReadonlyArray<RadioOption<number>> => [
	{
		value: RenderSpoilers.ON_CLICK,
		name: t(msg`On click`),
		desc: t(msg`Show spoiler content when clicked`),
	},
	{
		value: RenderSpoilers.IF_MODERATOR,
		name: t(msg`In channels I moderate`),
		desc: t(msg`Always show spoiler content in channels where you have the "Manage Messages" permission`),
	},
	{
		value: RenderSpoilers.ALWAYS,
		name: t(msg`Always`),
		desc: t(msg`Always show spoiler content`),
	},
];

export const DisplayTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const userSettings = UserSettingsStore;

	return (
		<>
			<SettingsTabSection
				title={t(msg`Media Display`)}
				description={t(
					msg`Control how images, videos and other media are shown. All media is resized and converted. Extremely large files that cannot be compressed into a preview will not embed regardless of these settings.`,
				)}
			>
				<div className={styles.sectionContent}>
					<SwitchGroup>
						<SwitchGroupItem
							label={t(msg`When posted as links to chat`)}
							value={userSettings.inlineEmbedMedia}
							onChange={(value) => UserSettingsActionCreators.update({inlineEmbedMedia: value})}
						/>
						<SwitchGroupItem
							label={t(msg`When uploaded directly to Fluxer`)}
							value={userSettings.inlineAttachmentMedia}
							onChange={(value) => UserSettingsActionCreators.update({inlineAttachmentMedia: value})}
						/>
					</SwitchGroup>
				</div>
			</SettingsTabSection>

			<SettingsTabSection
				title={t(msg`Link Previews`)}
				description={t(msg`Control how website links are previewed in chat`)}
			>
				<div className={styles.sectionContent}>
					<Switch
						label={t(msg`Show embeds and preview website links`)}
						value={userSettings.renderEmbeds}
						onChange={(value) => UserSettingsActionCreators.update({renderEmbeds: value})}
					/>
				</div>
			</SettingsTabSection>

			<SettingsTabSection title={t(msg`Reactions`)} description={t(msg`Configure emoji reactions on messages`)}>
				<div className={styles.sectionContent}>
					<Switch
						label={t(msg`Show emoji reactions on messages`)}
						value={userSettings.renderReactions}
						onChange={(value) => UserSettingsActionCreators.update({renderReactions: value})}
					/>
				</div>
			</SettingsTabSection>

			<SettingsTabSection
				title={t(msg`Spoiler Content`)}
				description={t(msg`Control how spoiler content is displayed`)}
			>
				<div className={styles.radioSection}>
					<div className={styles.radioLabelContainer}>
						<div className={styles.radioLabel}>
							<Trans>Show spoiler content</Trans>
						</div>
					</div>
					<RadioGroup
						options={spoilerOptions(t)}
						value={userSettings.renderSpoilers}
						onChange={(value) => UserSettingsActionCreators.update({renderSpoilers: value})}
						aria-label={t(msg`Show spoiler content`)}
					/>
				</div>
			</SettingsTabSection>
		</>
	);
});
