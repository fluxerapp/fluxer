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
import {SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import {MAX_BIO_LENGTH_PREMIUM} from '~/Constants';
import {Autocomplete, type AutocompleteOption, type AutocompleteType} from '~/components/channel/Autocomplete';
import {Textarea} from '~/components/form/Input';
import {ExpressionPickerPopout} from '~/components/popouts/ExpressionPickerPopout';
import {CharacterCounter} from '~/components/uikit/CharacterCounter/CharacterCounter';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {PlutoniumLink} from '~/components/uikit/PlutoniumLink/PlutoniumLink';
import {Popout} from '~/components/uikit/Popout/Popout';
import {useMarkdownKeybinds} from '~/hooks/useMarkdownKeybinds';
import {useTextareaAutocompleteKeyboard} from '~/hooks/useTextareaAutocompleteKeyboard';
import styles from './BioEditor.module.css';

interface BioEditorProps {
	value: string;
	onChange: (value: string) => void;
	onEmojiSelect: (emoji: any) => void;
	placeholder?: string;
	maxLength: number;
	disabled?: boolean;
	hasPremium: boolean;
	isPerGuildProfile: boolean;
	isMobile: boolean;
	errorMessage?: string;
	textareaRef: React.RefObject<HTMLTextAreaElement | null>;
	emojiPickerOpen: boolean;
	onEmojiPickerOpenChange: (open: boolean) => void;
	containerRef: React.RefObject<HTMLDivElement | null>;
	autocompleteQuery?: string;
	autocompleteOptions?: Array<AutocompleteOption>;
	autocompleteType?: AutocompleteType;
	selectedIndex?: number;
	isAutocompleteAttached?: boolean;
	setSelectedIndex?: React.Dispatch<React.SetStateAction<number>>;
	onCursorMove?: () => void;
	handleSelect?: (option: AutocompleteOption) => void;
	autocompleteZIndex?: number;
}

export const BioEditor = observer(
	({
		value,
		onChange,
		onEmojiSelect,
		placeholder,
		maxLength,
		disabled,
		hasPremium,
		isPerGuildProfile,
		isMobile,
		errorMessage,
		textareaRef,
		emojiPickerOpen,
		onEmojiPickerOpenChange,
		containerRef,
		autocompleteQuery,
		autocompleteOptions,
		autocompleteType,
		selectedIndex,
		isAutocompleteAttached,
		setSelectedIndex,
		onCursorMove,
		handleSelect,
		autocompleteZIndex,
	}: BioEditorProps) => {
		const {t} = useLingui();
		const [isFocused, setIsFocused] = React.useState(false);
		useMarkdownKeybinds(isFocused);
		const handleBioEmojiSelect = React.useCallback(
			(emoji: any) => {
				onEmojiSelect(emoji);
				onEmojiPickerOpenChange(false);
			},
			[onEmojiSelect, onEmojiPickerOpenChange],
		);

		const {handleKeyDown} = useTextareaAutocompleteKeyboard({
			isAutocompleteAttached: isAutocompleteAttached || false,
			autocompleteOptions: autocompleteOptions || [],
			selectedIndex: selectedIndex || 0,
			setSelectedIndex: setSelectedIndex || (() => {}),
			handleSelect: handleSelect || (() => {}),
		});

		return (
			<div>
				{isAutocompleteAttached && handleSelect && setSelectedIndex && (
					<Autocomplete
						type={autocompleteType || 'emoji'}
						onSelect={handleSelect}
						selectedIndex={selectedIndex || 0}
						options={autocompleteOptions || []}
						setSelectedIndex={setSelectedIndex}
						referenceElement={containerRef.current}
						query={autocompleteQuery || ''}
						zIndex={autocompleteZIndex}
					/>
				)}

				<div ref={containerRef}>
					<Textarea
						ref={textareaRef}
						label={t`About Me`}
						placeholder={placeholder}
						maxLength={maxLength}
						minRows={4}
						maxRows={4}
						showCharacterCount={true}
						value={value}
						onChange={(e) => onChange(e.target.value)}
						onFocus={() => setIsFocused(true)}
						onBlur={() => setIsFocused(false)}
						onKeyDown={handleKeyDown}
						onKeyUp={onCursorMove}
						onClick={onCursorMove}
						error={errorMessage}
						disabled={disabled}
						innerActionButton={
							isMobile ? (
								<FocusRing offset={-2} enabled={!disabled}>
									<button
										type="button"
										onClick={() => onEmojiPickerOpenChange(true)}
										className={clsx(styles.emojiButton, emojiPickerOpen && styles.emojiButtonActive)}
										disabled={disabled}
									>
										<SmileyIcon size={20} weight="fill" />
									</button>
								</FocusRing>
							) : (
								<Popout
									position="bottom-end"
									animationType="none"
									offsetMainAxis={8}
									offsetCrossAxis={0}
									onOpen={() => onEmojiPickerOpenChange(true)}
									onClose={() => onEmojiPickerOpenChange(false)}
									returnFocusRef={textareaRef}
									render={({onClose}) => (
										<ExpressionPickerPopout
											onEmojiSelect={(emoji) => {
												handleBioEmojiSelect(emoji);
												onClose();
											}}
											onClose={onClose}
											visibleTabs={['emojis']}
										/>
									)}
								>
									<FocusRing offset={-2} enabled={!disabled}>
										<button
											type="button"
											className={clsx(styles.emojiButton, emojiPickerOpen && styles.emojiButtonActive)}
											disabled={disabled}
										>
											<SmileyIcon size={20} weight="fill" />
										</button>
									</FocusRing>
								</Popout>
							)
						}
						characterCountTooltip={(_remaining, total, current) => (
							<CharacterCounter
								currentLength={current}
								maxLength={total}
								isPremium={hasPremium}
								premiumMaxLength={MAX_BIO_LENGTH_PREMIUM}
								onUpgradeClick={() => {
									PremiumModalActionCreators.open();
								}}
							/>
						)}
					/>
				</div>
				{!isPerGuildProfile && (
					<div className={styles.description}>
						{hasPremium ? (
							<Trans>You can use links and Markdown to format your text.</Trans>
						) : (
							<Trans>
								You can use links and Markdown to format your text. With <PlutoniumLink />, you can write up to{' '}
								{MAX_BIO_LENGTH_PREMIUM} characters.
							</Trans>
						)}
					</div>
				)}
				{isPerGuildProfile && (
					<div className={styles.description}>
						<Trans>You can use links and Markdown to format your text.</Trans>
					</div>
				)}
			</div>
		);
	},
);
