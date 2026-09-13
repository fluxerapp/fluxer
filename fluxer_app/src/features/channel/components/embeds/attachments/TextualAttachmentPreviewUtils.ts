// SPDX-License-Identifier: AGPL-3.0-or-later

import {isSupportedHighlightLanguage} from '@app/features/code_highlighting/utils/ArboriumHighlighting';
import {getLanguageFromAttachment, TEXT_PREVIEW_MAX_BYTES} from '@app/features/messaging/utils/AttachmentPreviewUtils';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {CSSProperties, MouseEvent} from 'react';

export const PREVIEW_LIMIT_KB = TEXT_PREVIEW_MAX_BYTES / 1024;
export const DEFAULT_PREVIEW_LINES = 6;
export const MAX_EXPANDED_PREVIEW_LINES = 100;
export const previewExpansionState = new Map<string | number, boolean>();
export type PreviewCountKind = 'line' | 'row';
export type TextualAttachmentRenderMode = 'code' | 'csv';

export type PreviewError =
	| {
			type: 'size';
	  }
	| {
			type: 'network';
	  };
export type PreviewStatus = 'idle' | 'loading' | 'loaded' | 'error';

export interface TextualAttachmentPreviewProps {
	attachment: MessageAttachment;
}

export interface TextualPreviewContextMenuProps {
	onDownload: () => void;
	onToggleWrapText: () => void;
	showWrapText?: boolean;
	wrapText: boolean;
}

export interface TextualAttachmentLanguageComboboxProps {
	defaultSearchQuery: string;
	onSelectLanguage: (languageCode: string) => void;
	selectedLanguage: string;
}

export interface TextualAttachmentCodePanelProps {
	canExpand: boolean;
	copyTextContent?: string | null;
	fillAvailableSpace?: boolean;
	highlightedHtml: string;
	isExpanded: boolean;
	previewError: PreviewError | null;
	status: PreviewStatus;
	textContent: string | null;
	visibleLineCount: number;
	wrapText: boolean;
	wrapperClassName?: string;
}

export interface PreviewPanelStyle extends CSSProperties {
	'--preview-visible-lines': string;
}

export interface TextualAttachmentPreviewFooterProps {
	attachment: MessageAttachment;
	canExpand: boolean;
	countKind?: PreviewCountKind;
	inferredLanguageCode: string;
	isExpanded: boolean;
	lineCount: number;
	onMoreOptions: (event: MouseEvent<HTMLButtonElement>) => void;
	onOpenFullscreen?: () => void;
	onSelectLanguage: (languageCode: string) => void;
	onToggleExpanded?: () => void;
	selectedLanguage: string;
	showExpandButton?: boolean;
	showLanguageButton?: boolean;
}

export interface TextualAttachmentPreviewModalProps {
	attachment: MessageAttachment;
	csvRows?: Array<Array<string>> | null;
	highlightedHtml: string;
	inferredLanguageCode: string;
	onClose: () => void;
	onMoreOptions: (event: MouseEvent<HTMLButtonElement>) => void;
	onSelectLanguage: (languageCode: string) => void;
	previewError: PreviewError | null;
	renderMode?: TextualAttachmentRenderMode;
	selectedLanguage: string;
	status: PreviewStatus;
	textContent: string | null;
	wrapText: boolean;
}

export function getAttachmentFileName(attachment: MessageAttachment): string {
	return attachment.filename ?? attachment.title ?? '';
}

function getFileExtension(fileName: string): string | null {
	const extension = fileName.split('.').pop()?.trim().toLowerCase();
	if (!extension) {
		return null;
	}
	return extension;
}

export function inferLanguageCodeFromAttachment(attachment: MessageAttachment): string {
	const mappedLanguage = getLanguageFromAttachment(attachment)?.trim().toLowerCase();
	if (mappedLanguage) {
		return mappedLanguage;
	}
	const fileName = getAttachmentFileName(attachment);
	const extensionCode = getFileExtension(fileName);
	if (extensionCode) {
		return extensionCode;
	}
	return 'plaintext';
}

export function getInitialSelectedLanguage(attachment: MessageAttachment, inferredLanguageCode: string): string {
	if (isSupportedHighlightLanguage(inferredLanguageCode)) {
		return inferredLanguageCode;
	}
	const mappedLanguage = getLanguageFromAttachment(attachment)?.trim().toLowerCase();
	if (mappedLanguage && isSupportedHighlightLanguage(mappedLanguage)) {
		return mappedLanguage;
	}
	return 'plaintext';
}

export function getLineCount(textContent: string | null): number {
	if (textContent == null || textContent.length === 0) {
		return 0;
	}
	return textContent.split(/\r\n|\r|\n/).length;
}

export function getVisibleLineCount(lineCount: number, isExpanded: boolean): number {
	if (!isExpanded) {
		return DEFAULT_PREVIEW_LINES;
	}
	if (lineCount <= 0) {
		return 1;
	}
	return Math.min(MAX_EXPANDED_PREVIEW_LINES, lineCount);
}
