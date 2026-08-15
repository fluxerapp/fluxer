// SPDX-License-Identifier: AGPL-3.0-or-later

type Px = `${number}px`;
type Rem = `${number}rem`;
type CssLength = Px | Rem;

export interface MessageDensityLayout {
	avatarSize: Rem;
	timestampFontSize: Rem;
}

export interface MessageLayoutSpec {
	cozy: MessageDensityLayout;
	dense: MessageDensityLayout & {
		timestampWidth: Rem;
		timestampHeight: Rem;
		gap: Rem;
		usernameGap: Rem;
	};
	gutter: Rem;
	spacingY: Rem;
	lineHeight: Rem;
	containerGap: Rem;
	containerPaddingY: Rem;
	mobileMargin: Rem;
	actionBarOffset: Rem;
	compactTableMaxInlineSize: string;
	compactTableMinCellWidth: Rem;
	compactMarkdown: {
		blockGap: Rem;
		blockquoteGap: Rem;
		blockquoteDividerMarginEnd: Rem;
		alertPaddingInline: Rem;
		alertPaddingBlock: Rem;
	};
	systemMessageIconSize: Rem;
	reply: {
		spacing: Rem;
		height: Rem;
		fontSize: Rem;
		spineWidth: Rem;
		spineRadius: Rem;
	};
	edited: {
		fontSize: Rem;
		labelFontSize: Rem;
	};
	icons: {
		sm: Rem;
		md: Rem;
		lg: Rem;
	};
	failedIndicator: {
		gap: Rem;
		fontSize: Rem;
	};
	typing: {
		gap: Rem;
		pillGap: Rem;
		pillPadding: Rem;
		avatarMargin: Rem;
		textFontSize: Rem;
	};
}

const CSS_LENGTH_PATTERN = /^-?(?:\d+|\d*\.\d+)(px|rem)$/u;

function parseLength(value: CssLength): {amount: number; unit: 'px' | 'rem'} {
	const match = CSS_LENGTH_PATTERN.exec(value);
	if (!match) {
		throw new Error(`Invalid CSS length: ${value}`);
	}
	return {amount: Number.parseFloat(value), unit: match[1] as 'px' | 'rem'};
}

function assertNonNegativeLength(name: string, value: CssLength): void {
	const {amount} = parseLength(value);
	if (!Number.isFinite(amount) || amount < 0) {
		throw new Error(`${name} must be a finite non-negative CSS length.`);
	}
}

function assertPositiveLength(name: string, value: CssLength): void {
	const {amount} = parseLength(value);
	if (!Number.isFinite(amount) || amount <= 0) {
		throw new Error(`${name} must be a finite positive CSS length.`);
	}
}

function assertSameUnitOrder(name: string, left: CssLength, right: CssLength): void {
	const a = parseLength(left);
	const b = parseLength(right);
	if (a.unit !== b.unit) {
		throw new Error(`${name} compares ${a.unit} with ${b.unit}; use matching units.`);
	}
	if (a.amount < b.amount) {
		throw new Error(`${name} must be at least ${right}.`);
	}
}

export const MESSAGE_LAYOUT_SPEC = {
	cozy: {
		avatarSize: '2.5rem',
		timestampFontSize: '0.75rem',
	},
	dense: {
		avatarSize: '1rem',
		timestampFontSize: '0.6875rem',
		timestampWidth: '3.5rem',
		timestampHeight: '1.25rem',
		gap: '0.25rem',
		usernameGap: '0.45rem',
	},
	gutter: '0.75rem',
	spacingY: '0.125rem',
	lineHeight: '1.375rem',
	containerGap: '0.25rem',
	containerPaddingY: '0.125rem',
	mobileMargin: '0.75rem',
	actionBarOffset: '3rem',
	compactTableMaxInlineSize: '100%',
	compactTableMinCellWidth: '5rem',
	compactMarkdown: {
		blockGap: '0.5rem',
		blockquoteGap: '0.25rem',
		blockquoteDividerMarginEnd: '0.35rem',
		alertPaddingInline: '0.75rem',
		alertPaddingBlock: '0.35rem',
	},
	systemMessageIconSize: '1.125rem',
	reply: {
		spacing: '0.25rem',
		height: '1.125rem',
		fontSize: '0.875rem',
		spineWidth: '0.125rem',
		spineRadius: '0.375rem',
	},
	edited: {
		fontSize: '0.75rem',
		labelFontSize: '0.625rem',
	},
	icons: {
		sm: '0.875rem',
		md: '1rem',
		lg: '1.25rem',
	},
	failedIndicator: {
		gap: '0.375rem',
		fontSize: '0.75rem',
	},
	typing: {
		gap: '0.35rem',
		pillGap: '0.2rem',
		pillPadding: '0.45rem',
		avatarMargin: '0.2rem',
		textFontSize: '0.6875rem',
	},
} as const satisfies MessageLayoutSpec;

export function assertMessageLayoutSpec(spec: MessageLayoutSpec): void {
	assertPositiveLength('cozy.avatarSize', spec.cozy.avatarSize);
	assertPositiveLength('dense.avatarSize', spec.dense.avatarSize);
	assertPositiveLength('gutter', spec.gutter);
	assertPositiveLength('lineHeight', spec.lineHeight);
	assertPositiveLength('dense.timestampWidth', spec.dense.timestampWidth);
	assertPositiveLength('dense.timestampHeight', spec.dense.timestampHeight);
	assertNonNegativeLength('dense.gap', spec.dense.gap);
	assertNonNegativeLength('dense.usernameGap', spec.dense.usernameGap);
	assertNonNegativeLength('spacingY', spec.spacingY);
	assertNonNegativeLength('containerGap', spec.containerGap);
	assertNonNegativeLength('containerPaddingY', spec.containerPaddingY);
	assertPositiveLength('compactTableMinCellWidth', spec.compactTableMinCellWidth);
	assertNonNegativeLength('compactMarkdown.blockGap', spec.compactMarkdown.blockGap);
	assertNonNegativeLength('compactMarkdown.blockquoteGap', spec.compactMarkdown.blockquoteGap);
	assertNonNegativeLength(
		'compactMarkdown.blockquoteDividerMarginEnd',
		spec.compactMarkdown.blockquoteDividerMarginEnd,
	);
	assertNonNegativeLength('compactMarkdown.alertPaddingInline', spec.compactMarkdown.alertPaddingInline);
	assertNonNegativeLength('compactMarkdown.alertPaddingBlock', spec.compactMarkdown.alertPaddingBlock);
	assertPositiveLength('systemMessageIconSize', spec.systemMessageIconSize);
	assertSameUnitOrder('dense.timestampHeight', spec.lineHeight, spec.dense.timestampHeight);
	assertSameUnitOrder('dense.timestampWidth', spec.dense.timestampWidth, spec.dense.gap);
	assertPositiveLength('reply.height', spec.reply.height);
	assertPositiveLength('reply.fontSize', spec.reply.fontSize);
	assertPositiveLength('reply.spineWidth', spec.reply.spineWidth);
	assertPositiveLength('reply.spineRadius', spec.reply.spineRadius);
	assertPositiveLength('edited.fontSize', spec.edited.fontSize);
	assertPositiveLength('edited.labelFontSize', spec.edited.labelFontSize);
	assertPositiveLength('icons.sm', spec.icons.sm);
	assertPositiveLength('icons.md', spec.icons.md);
	assertPositiveLength('icons.lg', spec.icons.lg);
	assertPositiveLength('failedIndicator.gap', spec.failedIndicator.gap);
	assertPositiveLength('failedIndicator.fontSize', spec.failedIndicator.fontSize);
	assertPositiveLength('typing.gap', spec.typing.gap);
	assertPositiveLength('typing.pillGap', spec.typing.pillGap);
	assertPositiveLength('typing.pillPadding', spec.typing.pillPadding);
	assertPositiveLength('typing.avatarMargin', spec.typing.avatarMargin);
	assertPositiveLength('typing.textFontSize', spec.typing.textFontSize);
	if (spec.compactTableMaxInlineSize !== '100%') {
		throw new Error('compactTableMaxInlineSize must stay bounded to the message body.');
	}
}

export function getMessageLayoutCssVariables(spec: MessageLayoutSpec = MESSAGE_LAYOUT_SPEC): Record<string, string> {
	assertMessageLayoutSpec(spec);
	const denseIndent = `calc(${spec.dense.timestampWidth} + ${spec.dense.gap})`;
	const avatarAlignOffset = `clamp(0px, calc((${spec.lineHeight} * 2 - ${spec.cozy.avatarSize}) / 2), 0.5rem)`;
	return {
		'--message-avatar-size': spec.cozy.avatarSize,
		'--message-avatar-size-compact': spec.dense.avatarSize,
		'--message-gutter': spec.gutter,
		'--message-spacing-y': spec.spacingY,
		'--message-line-height': spec.lineHeight,
		'--message-timestamp-font-size': spec.cozy.timestampFontSize,
		'--message-timestamp-compact-font-size': spec.dense.timestampFontSize,
		'--message-timestamp-compact-height': spec.dense.timestampHeight,
		'--message-compact-timestamp-width': spec.dense.timestampWidth,
		'--message-compact-gap': spec.dense.gap,
		'--message-compact-indent': denseIndent,
		'--message-compact-username-gap': spec.dense.usernameGap,
		'--message-compact-container-margin': denseIndent,
		'--message-compact-table-max-inline-size': spec.compactTableMaxInlineSize,
		'--message-compact-table-min-cell-width': spec.compactTableMinCellWidth,
		'--message-compact-markdown-block-gap': spec.compactMarkdown.blockGap,
		'--message-compact-markdown-blockquote-gap': spec.compactMarkdown.blockquoteGap,
		'--message-compact-markdown-blockquote-divider-margin-end': spec.compactMarkdown.blockquoteDividerMarginEnd,
		'--message-compact-markdown-alert-padding-inline': spec.compactMarkdown.alertPaddingInline,
		'--message-compact-markdown-alert-padding-block': spec.compactMarkdown.alertPaddingBlock,
		'--system-message-icon-size': spec.systemMessageIconSize,
		'--message-reply-spacing': spec.reply.spacing,
		'--message-reply-height': spec.reply.height,
		'--message-reply-font-size': spec.reply.fontSize,
		'--message-reply-spine-width': spec.reply.spineWidth,
		'--message-reply-spine-radius': spec.reply.spineRadius,
		'--message-container-gap': spec.containerGap,
		'--message-container-padding-y': spec.containerPaddingY,
		'--message-edited-font-size': spec.edited.fontSize,
		'--message-edited-label-font-size': spec.edited.labelFontSize,
		'--message-mobile-margin': spec.mobileMargin,
		'--message-action-bar-offset': spec.actionBarOffset,
		'--message-icon-size-sm': spec.icons.sm,
		'--message-icon-size-md': spec.icons.md,
		'--message-icon-size-lg': spec.icons.lg,
		'--message-failed-indicator-gap': spec.failedIndicator.gap,
		'--message-failed-indicator-font-size': spec.failedIndicator.fontSize,
		'--message-typing-gap': spec.typing.gap,
		'--message-typing-pill-gap': spec.typing.pillGap,
		'--message-typing-pill-padding': spec.typing.pillPadding,
		'--message-typing-avatar-margin': spec.typing.avatarMargin,
		'--message-typing-text-font-size': spec.typing.textFontSize,
		'--message-avatar-align-offset': avatarAlignOffset,
	};
}

assertMessageLayoutSpec(MESSAGE_LAYOUT_SPEC);
