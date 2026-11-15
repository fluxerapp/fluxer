// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {insertComposerEmoji} from '@app/features/lexical/composer/ComposerInsertion';
import {LexicalComposerInput} from '@app/features/lexical/composer/LexicalComposerInput';
import composerStyles from '@app/features/lexical/composer/LexicalMessageComposer.module.css';
import {type TriggerType, useLexicalAutocomplete} from '@app/features/lexical/composer/useLexicalAutocomplete';
import {useTextareaSegments} from '@app/features/messaging/hooks/useTextareaSegments';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {resolveTypedEmojiToken} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import {flxElementClassName} from '@app/lib/react';
import type {I18n} from '@lingui/core';
import type React from 'react';
import {useCallback, useId, useImperativeHandle, useRef, useState} from 'react';

export interface LexicalRichInputHandle {
	focus: () => void;

	insertEmoji: (emoji: FlatEmoji) => boolean;
}

export interface LexicalRichInputProps {
	initialValue?: string;
	initialSegments?: ReadonlyArray<MentionSegment>;
	placeholder: string;
	disabled?: boolean;
	channel?: Channel | null;
	allowedTriggers?: Array<TriggerType>;
	allowSpecialMentions?: boolean;
	markdown?: boolean;
	markdownParserFlags?: number;
	singleLine?: boolean;
	size?: 'chat' | 'form';
	maxLength?: number;
	maxWireLength?: number;
	onExceedMaxLength?: () => void;
	autocompleteAnchor?: HTMLElement | null;
	className?: string;
	id?: string;
	ariaLabel?: string;
	ariaLabelledBy?: string;
	ariaDescribedBy?: string;
	ariaErrorMessage?: string;
	ariaInvalid?: React.AriaAttributes['aria-invalid'];
	richInputRef?: React.Ref<LexicalRichInputHandle>;
	onChange?: (display: string, segments: Array<MentionSegment>, wire: string) => void;
	onSubmit?: () => void;
	submitOnEnter?: boolean;
	onKeyDown?: (event: React.KeyboardEvent<HTMLElement>) => void;
	onFocus?: () => void;
	onBlur?: () => void;
	i18n: I18n;
}

const ARROW_UP_UNHANDLED = (): boolean => false;
const SAFE_CHANNEL_TRIGGERS: Array<TriggerType> = ['emoji', 'mention', 'channel'];
const SAFE_CONTEXT_FREE_TRIGGERS: Array<TriggerType> = ['emoji'];

export const LexicalRichInput = ({
	initialValue = '',
	initialSegments = [],
	placeholder,
	disabled = false,
	channel = null,
	allowedTriggers,
	allowSpecialMentions = true,
	markdown = true,
	markdownParserFlags,
	singleLine = false,
	size = 'chat',
	maxLength,
	maxWireLength = maxLength,
	onExceedMaxLength,
	autocompleteAnchor,
	className,
	id,
	ariaLabel,
	ariaLabelledBy,
	ariaDescribedBy,
	ariaErrorMessage,
	ariaInvalid,
	richInputRef,
	onChange,
	onSubmit,
	submitOnEnter,
	onKeyDown,
	onFocus,
	onBlur,
	i18n,
}: LexicalRichInputProps) => {
	const [_value, setValue] = useState(initialValue);
	const handleRef = useRef<ComposerHandle | null>(null);
	const [containerElement, setContainerElement] = useState<HTMLElement | null>(null);
	const listboxId = useId();
	const channelGuildId: string | null = channel == null || channel.guildId == null ? null : channel.guildId;

	const emojiShortcodeResolver = useCallback(
		(shortcodeName: string) => resolveTypedEmojiToken(shortcodeName, channel, channelGuildId, i18n),
		[channel, channelGuildId, i18n],
	);

	const {segmentManagerRef, previousValueRef} = useTextareaSegments();

	const seededRef = useRef(false);
	if (!seededRef.current) {
		seededRef.current = true;
		segmentManagerRef.current.setSegments(initialSegments.map((segment) => ({...segment})));
		previousValueRef.current = initialValue;
	}

	const emitChange = useCallback(
		(display: string, segments: Array<MentionSegment>, wire: string) => {
			segmentManagerRef.current.setSegments(segments);
			previousValueRef.current = display;
			setValue(display);
			if (onChange != null) {
				onChange(display, segments, wire);
			}
		},
		[onChange, previousValueRef, segmentManagerRef],
	);

	const {
		autocompleteQuery,
		autocompleteOptions,
		autocompleteType,
		isSlotMenu,
		onCursorMove,
		handleSelect,
		specialMentionsAllowed,
	} = useLexicalAutocomplete({
		channel,
		handleRef,
		allowedTriggers: allowedTriggers ?? (channel == null ? SAFE_CONTEXT_FREE_TRIGGERS : SAFE_CHANNEL_TRIGGERS),
		allowSpecialMentions,
		allowMediaOptions: false,
		maxActualLength: maxLength,
		onExceedMaxLength,
		i18n,
	});

	const insertEmoji = useCallback(
		(emoji: FlatEmoji) =>
			insertComposerEmoji(handleRef.current, emoji, {
				maxWireLength: maxLength,
				onExceedMaxLength,
			}),
		[maxLength, onExceedMaxLength],
	);

	useImperativeHandle(
		richInputRef,
		(): LexicalRichInputHandle => ({
			focus: () => {
				const handle = handleRef.current;
				if (handle != null) {
					handle.focus();
				}
			},

			insertEmoji,
		}),
		[insertEmoji],
	);

	const handleEnter = useCallback(() => {
		if (onSubmit != null) {
			onSubmit();
		}
	}, [onSubmit]);

	return (
		<flx-lexical-rich-input
			ref={setContainerElement}
			className={flxElementClassName(
				composerStyles.richInputHost,
				size === 'form' && composerStyles.formSize,
				className,
			)}
			data-flx="lexical.composer.lexical-rich-input.flx-lexical-rich-input"
		>
			<LexicalComposerInput
				placeholder={placeholder}
				disabled={disabled}
				id={id}
				ariaLabel={ariaLabel}
				ariaLabelledBy={ariaLabelledBy}
				ariaDescribedBy={ariaDescribedBy}
				ariaErrorMessage={ariaErrorMessage}
				ariaInvalid={ariaInvalid}
				handleRef={handleRef}
				initialValue={initialValue}
				initialSegments={initialSegments}
				markdown={markdown}
				markdownParserFlags={markdownParserFlags}
				maxWireLength={maxWireLength}
				emojiShortcodeResolver={emojiShortcodeResolver}
				specialMentionsAllowed={specialMentionsAllowed}
				channelId={channel == null ? undefined : channel.id}
				guildId={channel == null ? undefined : channel.guildId}
				submitOnEnter={submitOnEnter ?? singleLine}
				autocompleteOptions={autocompleteOptions}
				autocompleteType={autocompleteType}
				autocompleteQuery={autocompleteQuery}
				autocompleteEnabled={!disabled}
				slotMenuActive={isSlotMenu}
				autocompleteReferenceElement={autocompleteAnchor == null ? containerElement : autocompleteAnchor}
				autocompleteListboxId={listboxId}
				onAutocompleteSelect={handleSelect}
				onChange={emitChange}
				onCursorMove={onCursorMove}
				onEnter={onSubmit == null ? undefined : handleEnter}
				onArrowUp={ARROW_UP_UNHANDLED}
				onKeyDown={onKeyDown}
				onFocus={onFocus}
				onBlur={onBlur}
				data-flx="lexical.composer.lexical-rich-input.lexical-composer-input.emit-change"
			/>
		</flx-lexical-rich-input>
	);
};
