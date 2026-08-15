// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {ExpressionPickerPopout} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {LexicalRichInput, type LexicalRichInputHandle} from '@app/features/lexical/composer/LexicalRichInput';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {useMarkdownKeybinds} from '@app/features/messaging/hooks/useMarkdownKeybinds';
import type {TriggerType} from '@app/features/messaging/utils/AutocompleteTriggerPolicy';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CharacterCounter} from '@app/features/ui/character_counter/CharacterCounter';
import formStyles from '@app/features/ui/components/form/FormInput.module.css';
import surfaceStyles from '@app/features/ui/components/form/FormSurface.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/BioEditor.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useRef, useState} from 'react';

const ABOUT_ME_DESCRIPTOR = msg({message: 'About me'});
const OPEN_EMOJI_PICKER_DESCRIPTOR = msg({message: 'Open emoji picker'});
const BIO_MARKDOWN_PARSER_FLAGS = getParserFlagsForContext(MarkdownContext.RESTRICTED_USER_BIO);
const BIO_ALLOWED_TRIGGERS: ReadonlyArray<TriggerType> = Object.freeze(['emoji']);
const COMPOSER_SURFACE_INTERACTIVE_SELECTOR =
	'[data-channel-textarea], button, a, input, textarea, select, [role="button"]';

interface BioEditorProps {
	initialValue: string;
	initialSegments: ReadonlyArray<MentionSegment>;
	hydrationKey: number;
	onChange: (display: string, segments: Array<MentionSegment>, wire: string) => void;
	onEmojiSelect: (emoji: FlatEmoji, shiftKey: boolean) => boolean;
	placeholder: string | null;
	actualLength: number;
	actualMaxLength: number;
	disabled: boolean;
	isMobile: boolean;
	errorMessage: string | null;
	composerRef: React.RefObject<LexicalRichInputHandle | null>;
	emojiPickerOpen: boolean;
	onEmojiPickerOpenChange: (open: boolean) => void;
}

export const BioEditor = observer(
	({
		initialValue,
		initialSegments,
		hydrationKey,
		onChange,
		onEmojiSelect,
		placeholder,
		actualLength,
		actualMaxLength,
		disabled,
		isMobile,
		errorMessage,
		composerRef,
		emojiPickerOpen,
		onEmojiPickerOpenChange,
	}: BioEditorProps) => {
		const {i18n} = useLingui();
		const [isFocused, setIsFocused] = useState(false);
		useMarkdownKeybinds(isFocused, {preserveEditableFocusActions: false});
		const editableId = useId();
		const labelId = `${editableId}-label`;
		const descriptionId = `${editableId}-description`;
		const errorId = `${editableId}-error`;
		const wrapperRef = useRef<HTMLDivElement | null>(null);
		const editableRef = useRef<HTMLDivElement | null>(null);
		useEffect(() => {
			const wrapper = wrapperRef.current;
			if (wrapper == null) {
				editableRef.current = null;
				return;
			}
			const editable = wrapper.querySelector<HTMLDivElement>('[data-channel-textarea]');
			if (editable == null) {
				editableRef.current = null;
				return;
			}
			editableRef.current = editable;
		}, [hydrationKey]);
		const handleBioEmojiSelect = useCallback(
			(emoji: FlatEmoji, shiftKey: boolean) => {
				const didInsert = onEmojiSelect(emoji, shiftKey);
				if (didInsert && shiftKey !== true) {
					onEmojiPickerOpenChange(false);
				}
				return didInsert;
			},
			[onEmojiPickerOpenChange, onEmojiSelect],
		);
		let resolvedPlaceholder = '';
		if (placeholder != null) {
			resolvedPlaceholder = placeholder;
		}
		const hasError = errorMessage != null && errorMessage.length > 0;
		let surfaceInteractionClass = formStyles.focusable;
		if (hasError) {
			surfaceInteractionClass = formStyles.error;
		}
		let describedBy = descriptionId;
		if (hasError) {
			describedBy = `${descriptionId} ${errorId}`;
		}
		let ariaErrorMessage: string | null = null;
		if (hasError) {
			ariaErrorMessage = errorId;
		}
		const emojiButton = isMobile ? (
			<FocusRing offset={-2} enabled={!disabled} data-flx="user.my-profile-tab.bio-editor.focus-ring">
				<button
					type="button"
					onClick={() => onEmojiPickerOpenChange(true)}
					className={clsx(styles.emojiButton, emojiPickerOpen && styles.emojiButtonActive)}
					disabled={disabled}
					aria-label={i18n._(OPEN_EMOJI_PICKER_DESCRIPTOR)}
					aria-haspopup="dialog"
					aria-expanded={emojiPickerOpen}
				>
					<SmileyIcon size={remFromPx(20)} weight="fill" />
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
				returnFocusRef={editableRef}
				render={({onClose}) => (
					<ExpressionPickerPopout
						onEmojiSelect={(emoji, shiftKey) => {
							const shiftPressed = shiftKey === true;
							const didInsert = handleBioEmojiSelect(emoji, shiftPressed);
							if (didInsert && !shiftPressed) {
								onClose();
							}
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
						aria-label={i18n._(OPEN_EMOJI_PICKER_DESCRIPTOR)}
						aria-haspopup="dialog"
						aria-expanded={emojiPickerOpen}
					>
						<SmileyIcon size={remFromPx(20)} weight="fill" />
					</button>
				</FocusRing>
			</Popout>
		);
		return (
			<div className="flx-element">
				<fieldset className={formStyles.fieldset}>
					<div className={formStyles.labelContainer}>
						<label
							id={labelId}
							htmlFor={editableId}
							onPointerDown={(event) => {
								if (disabled) {
									return;
								}
								event.preventDefault();
								const composer = composerRef.current;
								if (composer != null) {
									composer.focus();
								}
							}}
							className={formStyles.label}
						>
							{i18n._(ABOUT_ME_DESCRIPTOR)}
						</label>
					</div>
					<div className={formStyles.inputGroup}>
						<FocusRing within={true} ringTarget={wrapperRef} focusTarget={wrapperRef} offset={-2} enabled={!disabled}>
							<div
								ref={wrapperRef}
								className={clsx(formStyles.textareaWrapper, surfaceStyles.surface, surfaceInteractionClass)}
								onPointerDown={(event) => {
									if (disabled) {
										return;
									}
									const target = event.target;
									if (target instanceof Element && target.closest(COMPOSER_SURFACE_INTERACTIVE_SELECTOR) != null) {
										return;
									}
									event.preventDefault();
									const composer = composerRef.current;
									if (composer != null) {
										composer.focus();
									}
								}}
							>
								<LexicalRichInput
									key={hydrationKey}
									initialValue={initialValue}
									initialSegments={initialSegments}
									placeholder={resolvedPlaceholder}
									disabled={disabled}
									channel={null}
									allowedTriggers={[...BIO_ALLOWED_TRIGGERS]}
									markdown={true}
									markdownParserFlags={BIO_MARKDOWN_PARSER_FLAGS}
									singleLine={false}
									size="form"
									maxLength={actualMaxLength}
									className={styles.editor}
									autocompleteAnchor={wrapperRef.current}
									id={editableId}
									ariaLabelledBy={labelId}
									ariaInvalid={hasError}
									ariaErrorMessage={ariaErrorMessage == null ? undefined : ariaErrorMessage}
									ariaDescribedBy={describedBy}
									richInputRef={composerRef}
									onChange={onChange}
									onFocus={() => setIsFocused(true)}
									onBlur={() => setIsFocused(false)}
									i18n={i18n}
								/>
								<div className={formStyles.textareaActions}>
									{emojiButton}
									<div className={styles.characterCountContainer}>
										<CharacterCounter
											currentLength={actualLength}
											maxLength={actualMaxLength}
											canUpgrade={false}
											premiumMaxLength={actualMaxLength}
											onUpgradeClick={() => undefined}
										/>
									</div>
								</div>
							</div>
						</FocusRing>
						{hasError && (
							<span id={errorId} className={formStyles.errorText}>
								{errorMessage}
							</span>
						)}
					</div>
				</fieldset>
				<div id={descriptionId} className={styles.description}>
					<Trans>You can use links, emoji, and markdown.</Trans>
				</div>
			</div>
		);
	},
);
