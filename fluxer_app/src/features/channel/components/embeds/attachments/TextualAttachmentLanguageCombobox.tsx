// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreview.module.css';
import type {TextualAttachmentLanguageComboboxProps} from '@app/features/channel/components/embeds/attachments/TextualAttachmentPreviewUtils';
import {useHighlightLanguageOptions} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {Combobox as BaseCombobox} from '@base-ui/react/combobox';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon, CodeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

const CHANGE_LANGUAGE_DESCRIPTOR = msg({
	message: 'Change language',
	comment: 'Short label in the channel and chat textual attachment preview footer. Keep it concise.',
});
const SEARCH_LANGUAGE_CODE_DESCRIPTOR = msg({
	message: 'Search language code…',
	comment: 'Button or menu action label in the channel and chat textual attachment language popout. Keep it concise.',
});
const SYNTAX_HIGHLIGHTING_DESCRIPTOR = msg({
	message: 'Syntax highlighting',
	comment: 'Short label in the channel and chat textual attachment language popout. Keep it concise.',
});
const NO_RESULTS_FOUND_DESCRIPTOR = msg({
	message: 'No results found',
	comment: 'Empty state shown when a combobox search returns no matches.',
});

interface HighlightLanguageOption {
	value: string;
	label: string;
	canonicalCode: string;
}

export const TextualAttachmentLanguageCombobox = observer(function TextualAttachmentLanguageCombobox({
	defaultSearchQuery,
	onSelectLanguage,
	selectedLanguage,
}: TextualAttachmentLanguageComboboxProps) {
	const {i18n} = useLingui();
	const highlightLanguageOptions = useHighlightLanguageOptions();
	const languageOptions = useMemo<ReadonlyArray<HighlightLanguageOption>>(() => {
		const normalizedDefaultSearchQuery = defaultSearchQuery.trim().toLowerCase();
		const options = highlightLanguageOptions.map((languageOption) => ({
			value: languageOption.code,
			label: languageOption.code,
			canonicalCode: languageOption.canonicalCode,
		}));
		if (!normalizedDefaultSearchQuery) {
			return options;
		}
		return [...options].sort((left, right) => {
			const leftMatchesDefault =
				left.value === normalizedDefaultSearchQuery || left.canonicalCode === normalizedDefaultSearchQuery;
			const rightMatchesDefault =
				right.value === normalizedDefaultSearchQuery || right.canonicalCode === normalizedDefaultSearchQuery;
			return Number(rightMatchesDefault) - Number(leftMatchesDefault);
		});
	}, [defaultSearchQuery, highlightLanguageOptions]);
	const selectedOption = useMemo(
		() => languageOptions.find((option) => option.value === selectedLanguage) ?? null,
		[languageOptions, selectedLanguage],
	);
	const handleValueChange = useCallback(
		(option: HighlightLanguageOption | null) => {
			if (option != null) {
				onSelectLanguage(option.value);
			}
		},
		[onSelectLanguage],
	);
	const filterLanguageOption = useCallback((option: HighlightLanguageOption, query: string) => {
		const normalizedQuery = query.trim().toLowerCase();
		if (!normalizedQuery) {
			return true;
		}
		return (
			option.value.includes(normalizedQuery) ||
			option.label.includes(normalizedQuery) ||
			option.canonicalCode.includes(normalizedQuery)
		);
	}, []);
	const languageButtonLabel = i18n._(CHANGE_LANGUAGE_DESCRIPTOR);
	return (
		<BaseCombobox.Root<HighlightLanguageOption>
			items={languageOptions}
			value={selectedOption}
			onValueChange={handleValueChange}
			filter={filterLanguageOption}
			autoHighlight={true}
			itemToStringLabel={(option) => option.label}
			itemToStringValue={(option) => option.value}
			isItemEqualToValue={(option, selected) => option.value === selected.value}
			data-flx="channel.embeds.attachments.textual-attachment-language-combobox.base-combobox-root"
		>
			<Tooltip
				text={languageButtonLabel}
				data-flx="channel.embeds.attachments.textual-attachment-language-combobox.tooltip"
			>
				<FocusRing offset={-2} data-flx="channel.embeds.attachments.textual-attachment-language-combobox.focus-ring">
					<BaseCombobox.Trigger
						className={styles.controlButton}
						aria-label={languageButtonLabel}
						data-flx="channel.embeds.attachments.textual-attachment-language-combobox.trigger"
					>
						<CodeIcon
							size={remFromPx(18)}
							weight="regular"
							data-flx="channel.embeds.attachments.textual-attachment-language-combobox.code-icon"
						/>
					</BaseCombobox.Trigger>
				</FocusRing>
			</Tooltip>
			<BaseCombobox.Portal data-flx="channel.embeds.attachments.textual-attachment-language-combobox.portal">
				<BaseCombobox.Positioner
					className={styles.languagePositioner}
					positionMethod="fixed"
					side="top"
					align="end"
					sideOffset={8}
					collisionPadding={12}
					collisionAvoidance={{side: 'flip', align: 'shift', fallbackAxisSide: 'none'}}
					data-flx="channel.embeds.attachments.textual-attachment-language-combobox.positioner"
				>
					<BaseCombobox.Popup
						className={styles.languagePopup}
						aria-label={i18n._(SYNTAX_HIGHLIGHTING_DESCRIPTOR)}
						data-flx="channel.embeds.attachments.textual-attachment-language-combobox.popup"
					>
						<div
							className={styles.languageSearchWrap}
							data-flx="channel.embeds.attachments.textual-attachment-language-combobox.search-wrap"
						>
							<BaseCombobox.Input
								className={styles.languageSearchInput}
								placeholder={i18n._(SEARCH_LANGUAGE_CODE_DESCRIPTOR)}
								aria-label={i18n._(SYNTAX_HIGHLIGHTING_DESCRIPTOR)}
								{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
								data-flx="channel.embeds.attachments.textual-attachment-language-combobox.search-input"
							/>
						</div>
						<BaseCombobox.Empty
							className={styles.languageEmpty}
							data-flx="channel.embeds.attachments.textual-attachment-language-combobox.empty"
						>
							{i18n._(NO_RESULTS_FOUND_DESCRIPTOR)}
						</BaseCombobox.Empty>
						<BaseCombobox.List
							className={styles.languageList}
							data-flx="channel.embeds.attachments.textual-attachment-language-combobox.list"
						>
							{(option: HighlightLanguageOption) => (
								<BaseCombobox.Item
									key={option.value}
									value={option}
									className={(state) =>
										clsx(
											styles.languageItem,
											state.highlighted && styles.languageItemHighlighted,
											state.selected && styles.languageItemSelected,
										)
									}
									data-flx="channel.embeds.attachments.textual-attachment-language-combobox.item"
								>
									<span
										className={styles.languageOptionContent}
										data-flx="channel.embeds.attachments.textual-attachment-language-combobox.option-content"
									>
										<span
											className={styles.languageOptionCode}
											data-flx="channel.embeds.attachments.textual-attachment-language-combobox.option-code"
										>
											{option.value}
										</span>
										{option.canonicalCode !== option.value && (
											<span
												className={styles.languageOptionCanonical}
												data-flx="channel.embeds.attachments.textual-attachment-language-combobox.option-canonical"
											>
												{option.canonicalCode}
											</span>
										)}
									</span>
									<BaseCombobox.ItemIndicator
										className={styles.languageItemIndicator}
										data-flx="channel.embeds.attachments.textual-attachment-language-combobox.item-indicator"
									>
										<CheckIcon
											weight="bold"
											data-flx="channel.embeds.attachments.textual-attachment-language-combobox.item-check-icon"
										/>
									</BaseCombobox.ItemIndicator>
								</BaseCombobox.Item>
							)}
						</BaseCombobox.List>
					</BaseCombobox.Popup>
				</BaseCombobox.Positioner>
			</BaseCombobox.Portal>
		</BaseCombobox.Root>
	);
});
