// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/ThemeEmbed.module.css';
import {
	EmbedCard,
	EmbedSkeletonButton,
	EmbedSkeletonCircle,
	EmbedSkeletonSubtitle,
	EmbedSkeletonTitle,
} from '@app/features/messaging/components/embeds/embed_card/EmbedCard';
import cardStyles from '@app/features/messaging/components/embeds/embed_card/EmbedCard.module.css';
import {useEmbedSkeletonOverride} from '@app/features/messaging/components/embeds/embed_card/useEmbedSkeletonOverride';
import * as ThemeCommands from '@app/features/theme/commands/ThemeCommands';
import {useThemeExists} from '@app/features/theme/hooks/useThemeExists';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PaletteIcon, QuestionIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const THIS_THEME_IS_NO_LONGER_AVAILABLE_DESCRIPTOR = msg({
	message: 'This theme is no longer available.',
	comment: 'Description text in the channel and chat theme embed.',
});
const SHARED_THEME_DESCRIPTOR = msg({
	message: 'Shared theme',
	comment: 'Button or menu action label in the channel and chat theme embed. Keep it concise.',
});
const IMPORT_THEME_DESCRIPTOR = msg({
	message: 'Import theme',
	comment: 'Short label in the channel and chat theme embed. Keep it concise.',
});
const THEME_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Theme unavailable',
	comment: 'Short label in the channel and chat theme embed. Keep it concise.',
});
const IMPORT_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Import unavailable',
	comment: 'Short label in the channel and chat theme embed. Keep it concise.',
});

interface ThemeEmbedProps {
	themeId: string;
}

export const ThemeEmbed = observer(function ThemeEmbed({themeId}: ThemeEmbedProps) {
	const {i18n} = useLingui();
	const runtimeSnapshot = RuntimeConfig.getSnapshot();
	const status = useThemeExists(themeId, runtimeSnapshot);
	const shouldForceSkeleton = useEmbedSkeletonOverride();
	const handleImport = () => {
		ThemeCommands.openAcceptModal(themeId, i18n, runtimeSnapshot);
	};
	if (shouldForceSkeleton || status === 'loading') {
		return <ThemeLoadingState data-flx="channel.theme-embed.theme-loading-state" />;
	}
	if (status === 'error') {
		return (
			<ThemeUnavailableError
				message={i18n._(THIS_THEME_IS_NO_LONGER_AVAILABLE_DESCRIPTOR)}
				data-flx="channel.theme-embed.theme-unavailable-error"
			/>
		);
	}
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div className={`${styles.iconCircle} ${styles.iconCircleActive}`} data-flx="channel.theme-embed.icon-circle">
					<PaletteIcon
						size={24}
						weight="bold"
						className={styles.iconOnBrand}
						data-flx="channel.theme-embed.icon-on-brand"
					/>
				</div>
			}
			title={
				<h3 className={`${cardStyles.title} ${cardStyles.titlePrimary}`} data-flx="channel.theme-embed.h3">
					{i18n._(SHARED_THEME_DESCRIPTOR)}
				</h3>
			}
			subtitle={
				<span className={cardStyles.helpText} data-flx="channel.theme-embed.span">
					<Trans>You've got CSS!</Trans>
				</span>
			}
			footer={
				<Button
					variant="primary"
					fitContainer
					matchSkeletonHeight
					onClick={handleImport}
					data-flx="channel.theme-embed.button.import"
				>
					{i18n._(IMPORT_THEME_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.theme-embed.embed-card"
		/>
	);
});
const ThemeLoadingState = observer(() => {
	return (
		<EmbedCard
			splashURL={null}
			icon={<EmbedSkeletonCircle data-flx="channel.theme-embed.theme-loading-state.embed-skeleton-circle" />}
			title={<EmbedSkeletonTitle data-flx="channel.theme-embed.theme-loading-state.embed-skeleton-title" />}
			subtitle={<EmbedSkeletonSubtitle data-flx="channel.theme-embed.theme-loading-state.embed-skeleton-subtitle" />}
			footer={<EmbedSkeletonButton data-flx="channel.theme-embed.theme-loading-state.embed-skeleton-button" />}
			data-flx="channel.theme-embed.theme-loading-state.embed-card"
		/>
	);
});
const ThemeUnavailableError = observer(({message}: {message: string | null}) => {
	const {i18n} = useLingui();
	return (
		<EmbedCard
			splashURL={null}
			icon={
				<div className={cardStyles.iconCircleDisabled} data-flx="channel.theme-embed.theme-unavailable-error.div">
					<QuestionIcon
						className={cardStyles.iconError}
						data-flx="channel.theme-embed.theme-unavailable-error.question-icon"
					/>
				</div>
			}
			title={
				<h3
					className={`${cardStyles.title} ${cardStyles.titleDanger}`}
					data-flx="channel.theme-embed.theme-unavailable-error.h3"
				>
					{i18n._(THEME_UNAVAILABLE_DESCRIPTOR)}
				</h3>
			}
			subtitle={
				<span className={cardStyles.helpText} data-flx="channel.theme-embed.theme-unavailable-error.span">
					{message ?? i18n._(THIS_THEME_IS_NO_LONGER_AVAILABLE_DESCRIPTOR)}
				</span>
			}
			footer={
				<Button
					variant="primary"
					fitContainer
					matchSkeletonHeight
					disabled
					data-flx="channel.theme-embed.theme-unavailable-error.button"
				>
					{i18n._(IMPORT_UNAVAILABLE_DESCRIPTOR)}
				</Button>
			}
			data-flx="channel.theme-embed.theme-unavailable-error.embed-card"
		/>
	);
});
