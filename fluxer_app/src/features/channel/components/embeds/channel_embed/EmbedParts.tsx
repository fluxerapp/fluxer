// SPDX-License-Identifier: AGPL-3.0-or-later

import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {EmbedLink} from '@app/features/channel/components/embeds/channel_embed/EmbedLink';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import type {EmbedAuthor, EmbedField, EmbedFooter} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FC, useMemo} from 'react';

const STILL_EMBED_ICON_OPTIONS = {format: 'webp', animated: false} as const;

const resolveEmbedIconSrc = (proxyIconUrl: string | undefined, allowMotion: boolean): string | undefined => {
	if (!proxyIconUrl) return undefined;
	return buildMediaProxyURL(proxyIconUrl, allowMotion ? {} : STILL_EMBED_ICON_OPTIONS);
};

export const EmbedProvider: FC<{provider?: EmbedAuthor}> = observer(({provider}) => {
	if (!provider) return null;
	return (
		<div className={styles.embedProvider} data-flx="channel.embeds.embed.embed-provider.embed-provider">
			{provider.url ? (
				<EmbedLink url={provider.url} data-flx="channel.embeds.channel-embed.embed-parts.embed-provider.embed-link">
					{provider.name}
				</EmbedLink>
			) : (
				<span data-flx="channel.embeds.embed.embed-provider.span">{provider.name}</span>
			)}
		</div>
	);
});
export const EmbedAuthorComponent: FC<{author?: EmbedAuthor}> = observer(({author}) => {
	const allowIconMotion = useShouldAnimate({kind: 'gif'});
	const iconSrc = resolveEmbedIconSrc(author?.proxy_icon_url, allowIconMotion);
	if (!author) return null;
	return (
		<div className={styles.embedAuthor} data-flx="channel.embeds.embed.embed-author-component.embed-author">
			{iconSrc && (
				<img
					alt=""
					className={styles.embedAuthorIcon}
					src={iconSrc}
					width={24}
					height={24}
					data-flx="channel.embeds.embed.embed-author-component.embed-author-icon"
				/>
			)}
			{author.url ? (
				<EmbedLink
					className={clsx(styles.embedAuthorName, styles.embedAuthorNameLink)}
					url={author.url}
					data-flx="channel.embeds.channel-embed.embed-parts.embed-author-component.embed-author-name"
				>
					{author.name}
				</EmbedLink>
			) : (
				<span
					className={styles.embedAuthorName}
					data-flx="channel.embeds.embed.embed-author-component.embed-author-name--2"
				>
					{author.name}
				</span>
			)}
		</div>
	);
});
export const EmbedTitle: FC<{title?: string; url?: string; messageId?: string; channelId?: string}> = observer(
	({title, url, messageId, channelId}) => {
		if (title == null || title.length === 0) return null;
		const options = {context: MarkdownContext.RESTRICTED_INLINE_REPLY, messageId, channelId};
		return (
			<div className={styles.embedTitle} data-flx="channel.embeds.embed.embed-title.embed-title">
				{url ? (
					<EmbedLink url={url} data-flx="channel.embeds.channel-embed.embed-parts.embed-title.embed-link">
						<SafeMarkdown content={title} options={options} data-flx="channel.embeds.embed.embed-title.safe-markdown" />
					</EmbedLink>
				) : (
					<span data-flx="channel.embeds.embed.embed-title.span">
						<SafeMarkdown
							content={title}
							options={options}
							data-flx="channel.embeds.embed.embed-title.safe-markdown--2"
						/>
					</span>
				)}
			</div>
		);
	},
);
export const EmbedDescription: FC<{
	messageId?: string;
	channelId?: string;
	description?: string;
}> = observer(({messageId, channelId, description}) => {
	if (!description) return null;
	return (
		<div className={styles.embedDescription} data-flx="channel.embeds.embed.embed-description.embed-description">
			<SafeMarkdown
				content={description}
				options={{context: MarkdownContext.RESTRICTED_EMBED_DESCRIPTION, messageId, channelId}}
				data-flx="channel.embeds.embed.embed-description.safe-markdown"
			/>
		</div>
	);
});
const MAX_INLINE_PER_ROW = 3;
const groupFields = (fields?: ReadonlyArray<EmbedField>): Array<ReadonlyArray<EmbedField>> => {
	if (!fields?.length) return [];
	const groupedFields: Array<ReadonlyArray<EmbedField>> = [];
	let currentGroup: Array<EmbedField> = [];
	for (const field of fields) {
		if (field.inline) {
			currentGroup.push(field);
			if (currentGroup.length === MAX_INLINE_PER_ROW) {
				groupedFields.push(currentGroup);
				currentGroup = [];
			}
		} else {
			if (currentGroup.length > 0) {
				groupedFields.push(currentGroup);
				currentGroup = [];
			}
			groupedFields.push([field]);
		}
	}
	if (currentGroup.length > 0) {
		groupedFields.push(currentGroup);
	}
	return groupedFields;
};
export const EmbedFields: FC<{fields?: ReadonlyArray<EmbedField>; messageId?: string; channelId?: string}> = observer(
	({fields, messageId, channelId}) => {
		const groupedFields = useMemo(() => groupFields(fields), [fields]);
		if (groupedFields.length === 0) return null;
		return (
			<div className={styles.embedFields} data-flx="channel.embeds.embed.embed-fields.embed-fields">
				{groupedFields.map((group, groupIndex) => {
					const groupLength = group.length;
					return group.map(({name, value}, index) => {
						const span = groupLength === 1 ? 12 : groupLength === 2 ? 6 : 4;
						const gridColumnStart = index * span + 1;
						const gridColumnEnd = gridColumnStart + span;
						return (
							<div
								className={styles.embedField}
								key={`${groupIndex}-${index}`}
								style={{gridColumn: `${gridColumnStart} / ${gridColumnEnd}`}}
								data-flx="channel.embeds.embed.embed-fields.embed-field"
							>
								<div className={styles.embedFieldName} data-flx="channel.embeds.embed.embed-fields.embed-field-name">
									<SafeMarkdown
										content={name}
										options={{context: MarkdownContext.RESTRICTED_INLINE_REPLY, messageId, channelId}}
										data-flx="channel.embeds.embed.embed-fields.safe-markdown"
									/>
								</div>
								<div className={styles.embedFieldValue} data-flx="channel.embeds.embed.embed-fields.embed-field-value">
									<SafeMarkdown
										content={value}
										options={{context: MarkdownContext.RESTRICTED_EMBED_DESCRIPTION, messageId, channelId}}
										data-flx="channel.embeds.embed.embed-fields.safe-markdown--2"
									/>
								</div>
							</div>
						);
					});
				})}
			</div>
		);
	},
);
export const EmbedFooterComponent: FC<{
	timestamp?: Date;
	footer?: EmbedFooter;
	messageId?: string;
	channelId?: string;
}> = observer(({timestamp, footer, messageId, channelId}) => {
	const {i18n} = useLingui();
	const allowIconMotion = useShouldAnimate({kind: 'gif'});
	const formattedTimestamp = timestamp ? DateUtils.getRelativeDateString(timestamp, i18n) : undefined;
	const iconSrc = resolveEmbedIconSrc(footer?.proxy_icon_url, allowIconMotion);
	if (!(footer || formattedTimestamp)) return null;
	return (
		<div
			className={clsx(styles.embedFooter, footer?.proxy_icon_url && styles.hasThumbnail)}
			data-flx="channel.embeds.embed.embed-footer-component.embed-footer"
		>
			{iconSrc && (
				<img
					alt=""
					className={styles.embedFooterIcon}
					src={iconSrc}
					width={20}
					height={20}
					data-flx="channel.embeds.embed.embed-footer-component.embed-footer-icon"
				/>
			)}
			<div className={styles.embedFooterText} data-flx="channel.embeds.embed.embed-footer-component.embed-footer-text">
				{footer?.text && (
					<SafeMarkdown
						content={footer.text}
						options={{context: MarkdownContext.RESTRICTED_INLINE_REPLY, messageId, channelId}}
						data-flx="channel.embeds.embed.embed-footer-component.safe-markdown"
					/>
				)}
				{formattedTimestamp && (
					<>
						{footer?.text && (
							<>
								{'​'}
								<div
									className={styles.embedFooterSeparator}
									data-flx="channel.embeds.embed.embed-footer-component.embed-footer-separator"
								/>
								{'​'}
							</>
						)}
						<span data-flx="channel.embeds.embed.embed-footer-component.span">{formattedTimestamp}</span>
					</>
				)}
			</div>
		</div>
	);
});
