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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {TimeFormatTypes} from '~/Constants';
import {Switch} from '~/components/form/Switch';
import {SettingsTabContainer, SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import AccessibilityStore from '~/stores/AccessibilityStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import * as EmojiUtils from '~/utils/EmojiUtils';
import * as LocaleUtils from '~/utils/LocaleUtils';
import * as NativeUtils from '~/utils/NativeUtils';
import styles from './LanguageTab.module.css';

const LanguageTab = observer(() => {
	const {t} = useLingui();
	const currentLocale = LocaleUtils.getCurrentLocale();
	const availableLocales = LocaleUtils.getSortedLocales();
	const {timeFormat} = UserSettingsStore;
	const isDesktop = NativeUtils.isDesktop();

	const getAutoTimeFormatDescription = () => {
		const appLocale = UserSettingsStore.getLocale();
		const browserLocale = typeof navigator !== 'undefined' ? navigator.language : appLocale;
		const effectiveLocale = AccessibilityStore.useBrowserLocaleForTimeFormat ? browserLocale : appLocale;

		const localeUses12Hour = (locale: string): boolean => {
			const lang = locale.toLowerCase();
			const twelveHourLocales = [
				'en-us',
				'en-ca',
				'en-au',
				'en-nz',
				'en-ph',
				'en-in',
				'en-pk',
				'en-bd',
				'en-za',
				'es-mx',
				'es-co',
				'ar',
				'hi',
				'bn',
				'ur',
				'fil',
				'tl',
			];
			return twelveHourLocales.some((l) => lang.startsWith(l));
		};

		const uses12Hour = localeUses12Hour(effectiveLocale);
		const sampleDate = new Date(2025, 0, 1, 14, 30, 0);
		const format = sampleDate.toLocaleString(effectiveLocale, {
			hour: 'numeric',
			minute: 'numeric',
			hour12: uses12Hour,
		});

		if (AccessibilityStore.useBrowserLocaleForTimeFormat) {
			return isDesktop
				? t`System locale (${browserLocale}): ${format}`
				: t`Browser locale (${browserLocale}): ${format}`;
		}

		return t`App language (${appLocale}): ${format}`;
	};

	const get12HourExample = () => {
		const locale = UserSettingsStore.getLocale();
		const sampleDate = new Date(2025, 0, 1, 14, 30, 0);
		return sampleDate.toLocaleString(locale, {hour: 'numeric', minute: 'numeric', hour12: true});
	};

	const get24HourExample = () => {
		const locale = UserSettingsStore.getLocale();
		const sampleDate = new Date(2025, 0, 1, 14, 30, 0);
		return sampleDate.toLocaleString(locale, {hour: 'numeric', minute: 'numeric', hour12: false});
	};

	const timeFormatOptions: ReadonlyArray<RadioOption<number>> = [
		{value: TimeFormatTypes.AUTO, name: t`Auto`, desc: getAutoTimeFormatDescription()},
		{value: TimeFormatTypes.TWELVE_HOUR, name: t`12-hour`, desc: get12HourExample()},
		{value: TimeFormatTypes.TWENTY_FOUR_HOUR, name: t`24-hour`, desc: get24HourExample()},
	];

	const handleLocaleChange = (newLocale: string) => {
		LocaleUtils.setLocale(newLocale);
	};

	const localeOptions: ReadonlyArray<RadioOption<string>> = availableLocales.map((locale) => ({
		value: locale.code,
		name: locale.nativeName,
		desc: locale.name,
	}));

	const renderLanguageContent = (option: RadioOption<string>, checked: boolean) => {
		const localeInfo = availableLocales.find((locale) => locale.code === option.value)!;
		const isEnGB = localeInfo.code === 'en-GB';

		const isLightTheme = document.documentElement.classList.contains('theme-light');
		const highlightClass = checked && !isLightTheme ? styles.languageNameHighlighted : undefined;

		const nameClass = clsx(styles.languageName, highlightClass);
		const descClass = clsx(styles.languageCode, checked && !isLightTheme ? styles.languageCodeHighlighted : undefined);

		const flagImg = (
			<img
				src={EmojiUtils.getEmojiURL(localeInfo.flag) ?? undefined}
				alt={`${localeInfo.name} flag`}
				className={styles.flagImage}
				draggable={false}
			/>
		);

		return (
			<div className={styles.languageOption}>
				<span className={nameClass}>{localeInfo.nativeName}</span>
				<div className={styles.languageDetails}>
					<span className={descClass}>{localeInfo.name}</span>
					{isEnGB ? (
						<Tooltip
							text={() => (
								<span className={styles.tooltipContent}>
									<span className={styles.tooltipText}>
										<Trans>For British eyes only...</Trans>
									</span>
								</span>
							)}
						>
							{flagImg}
						</Tooltip>
					) : (
						flagImg
					)}
				</div>
			</div>
		);
	};

	return (
		<SettingsTabContainer>
			<SettingsTabSection
				title={<Trans>Time Format</Trans>}
				description={<Trans>Choose how times are displayed throughout the app.</Trans>}
			>
				<RadioGroup
					options={timeFormatOptions}
					value={timeFormat}
					onChange={(value) => UserSettingsActionCreators.update({timeFormat: value})}
					aria-label={t`Time format selection`}
				/>
				{timeFormat === TimeFormatTypes.AUTO && (
					<div className={styles.switchWrapper}>
						<Switch
							label={isDesktop ? t`Use system locale for time format` : t`Use browser locale for time format`}
							description={
								isDesktop
									? t`When enabled, uses your computer's locale to determine 12/24-hour format instead of the app's language setting.`
									: t`When enabled, uses your browser's locale to determine 12/24-hour format instead of the app's language setting.`
							}
							value={AccessibilityStore.useBrowserLocaleForTimeFormat}
							onChange={(value) => AccessibilityActionCreators.update({useBrowserLocaleForTimeFormat: value})}
						/>
					</div>
				)}
			</SettingsTabSection>

			<div className={styles.notice}>
				<p className={styles.noticeText}>
					<Trans>
						All translations are currently LLM-generated with minimal human revision. We'd love to get real people to
						help us localize Fluxer into your language! To do so, send an email to{' '}
						<a href="mailto:i18n@fluxer.app" className={styles.link}>
							i18n@fluxer.app
						</a>{' '}
						and we'll be happy to accept your contributions.
					</Trans>
				</p>
			</div>

			<SettingsTabSection
				title={<Trans>Language Settings</Trans>}
				description={<Trans>Choose your preferred language for the interface.</Trans>}
			>
				<RadioGroup
					options={localeOptions}
					value={currentLocale}
					onChange={handleLocaleChange}
					renderContent={renderLanguageContent}
					aria-label={t`Select interface language`}
				/>
			</SettingsTabSection>
		</SettingsTabContainer>
	);
});

export default LanguageTab;
