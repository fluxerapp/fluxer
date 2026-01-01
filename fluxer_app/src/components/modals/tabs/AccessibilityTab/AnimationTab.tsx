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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {StickerAnimationOptions} from '~/Constants';
import {Switch} from '~/components/form/Switch';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import AccessibilityOverrideStore, {type AnimationOverrides} from '~/stores/AccessibilityOverrideStore';
import AccessibilityStore from '~/stores/AccessibilityStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from './AnimationTab.module.css';

export const AnimationTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const mobileLayout = MobileLayoutStore;
	const mobileStickerAnimationOverridden = AccessibilityStore.mobileStickerAnimationOverridden;
	const mobileGifAutoPlayOverridden = AccessibilityStore.mobileGifAutoPlayOverridden;

	const stickerAnimationOptions = React.useMemo(
		() =>
			[
				{value: StickerAnimationOptions.ALWAYS_ANIMATE, name: t`Always animate`, desc: t`Stickers will always animate`},
				{
					value: StickerAnimationOptions.ANIMATE_ON_INTERACTION,
					name: t`Animate on interaction`,
					desc: mobileLayout.enabled
						? t`Stickers will animate when you press them`
						: t`Stickers will animate when you hover or interact with them`,
				},
				{value: StickerAnimationOptions.NEVER_ANIMATE, name: t`Never animate`, desc: t`Stickers will never animate`},
			] as ReadonlyArray<RadioOption<number>>,
		[mobileLayout.enabled, t],
	);

	const getOverrideDescription = (
		setting: 'gif_auto_play' | 'animate_emoji' | 'animate_stickers',
	): string | undefined => {
		if (AccessibilityOverrideStore.isOverriddenByReducedMotion(setting)) {
			return t`This setting is currently overridden by your reduced motion preferences`;
		}
		return;
	};

	const handleAnimationSettingChange = (
		setting: 'gif_auto_play' | 'animate_emoji' | 'animate_stickers',
		_value: unknown,
		updateAction: () => void,
	) => {
		const dirtyKey: keyof AnimationOverrides =
			setting === 'gif_auto_play'
				? 'gifAutoPlayDirty'
				: setting === 'animate_emoji'
					? 'animateEmojiDirty'
					: 'animateStickersDirty';
		AccessibilityOverrideStore.markDirty(dirtyKey);
		updateAction();
	};

	return (
		<>
			<Switch
				label={t`Play animated emojis`}
				description={getOverrideDescription('animate_emoji')}
				value={UserSettingsStore.getAnimateEmoji()}
				onChange={(value) => {
					if (mobileLayout.enabled) {
						AccessibilityActionCreators.update({
							mobileAnimateEmojiOverridden: true,
							mobileAnimateEmojiValue: value,
						});
					} else {
						handleAnimationSettingChange('animate_emoji', value, () =>
							UserSettingsActionCreators.update({animateEmoji: value}),
						);
					}
				}}
			/>

			<Switch
				label={mobileLayout.enabled ? t`Automatically play GIFs` : t`Automatically play GIFs when Fluxer is focused`}
				description={
					mobileLayout.enabled && !mobileGifAutoPlayOverridden
						? t`Defaults to off on mobile to preserve battery life and data usage.`
						: getOverrideDescription('gif_auto_play')
				}
				value={UserSettingsStore.getGifAutoPlay()}
				onChange={(value) => {
					if (mobileLayout.enabled) {
						AccessibilityActionCreators.update({
							mobileGifAutoPlayOverridden: true,
							mobileGifAutoPlayValue: value,
						});
					} else {
						handleAnimationSettingChange('gif_auto_play', value, () =>
							UserSettingsActionCreators.update({gifAutoPlay: value}),
						);
					}
				}}
			/>

			<div className={styles.radioSection}>
				<div className={styles.radioHeader}>
					<div className={styles.radioLabel}>
						<Trans>Sticker animations</Trans>
					</div>
					{mobileLayout.enabled && !mobileStickerAnimationOverridden ? (
						<p className={styles.radioDescription}>
							{t`Defaults to animate on interaction on mobile to preserve battery life.`}
						</p>
					) : (
						getOverrideDescription('animate_stickers') && (
							<p className={styles.radioDescription}>{getOverrideDescription('animate_stickers')}</p>
						)
					)}
				</div>
				<RadioGroup
					aria-label={t`Sticker animation preference`}
					options={stickerAnimationOptions}
					value={UserSettingsStore.getAnimateStickers()}
					onChange={(value) => {
						if (mobileLayout.enabled) {
							AccessibilityActionCreators.update({
								mobileStickerAnimationOverridden: true,
								mobileStickerAnimationValue: value,
							});
						} else {
							handleAnimationSettingChange('animate_stickers', value, () =>
								UserSettingsActionCreators.update({animateStickers: value}),
							);
						}
					}}
				/>
			</div>
		</>
	);
});
