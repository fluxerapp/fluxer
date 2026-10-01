// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/premium/components/plutonium_page/PlutoniumPage.module.css';
import {
	SHOWCASE_EXPRESSIONS_BODY_DESCRIPTOR,
	SHOWCASE_EXPRESSIONS_TITLE_DESCRIPTOR,
	SHOWCASE_PROFILE_BODY_DESCRIPTOR,
	SHOWCASE_PROFILE_TITLE_DESCRIPTOR,
	SHOWCASE_STREAM_BODY_DESCRIPTOR,
	SHOWCASE_STREAM_TITLE_DESCRIPTOR,
	SHOWCASE_UPLOAD_BODY_DESCRIPTOR,
	SHOWCASE_UPLOAD_TITLE_DESCRIPTOR,
	TAG_FOOTNOTE_MARKER_DESCRIPTOR,
} from '@app/features/premium/components/plutonium_page/PlutoniumPageMessages';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import avatarLsf from '@app/media/images/plutonium/avatar-lsf.webp';
import bannerLsf from '@app/media/images/plutonium/banner-lsf.webp';
import emojiCatvibe from '@app/media/images/plutonium/emoji-catvibe.webp';
import emojiCatwave from '@app/media/images/plutonium/emoji-catwave.webp';
import expressions640Avif from '@app/media/images/plutonium/feature-perk-expressions-640w.avif';
import expressions640Webp from '@app/media/images/plutonium/feature-perk-expressions-640w.webp';
import expressions1120Avif from '@app/media/images/plutonium/feature-perk-expressions-1120w.avif';
import expressions1120Webp from '@app/media/images/plutonium/feature-perk-expressions-1120w.webp';
import expressions1600Avif from '@app/media/images/plutonium/feature-perk-expressions-1600w.avif';
import expressions1600Webp from '@app/media/images/plutonium/feature-perk-expressions-1600w.webp';
import profile640Avif from '@app/media/images/plutonium/feature-perk-profile-640w.avif';
import profile640Webp from '@app/media/images/plutonium/feature-perk-profile-640w.webp';
import profile1120Avif from '@app/media/images/plutonium/feature-perk-profile-1120w.avif';
import profile1120Webp from '@app/media/images/plutonium/feature-perk-profile-1120w.webp';
import profile1600Avif from '@app/media/images/plutonium/feature-perk-profile-1600w.avif';
import profile1600Webp from '@app/media/images/plutonium/feature-perk-profile-1600w.webp';
import stream640Avif from '@app/media/images/plutonium/feature-perk-stream-640w.avif';
import stream640Webp from '@app/media/images/plutonium/feature-perk-stream-640w.webp';
import stream1120Avif from '@app/media/images/plutonium/feature-perk-stream-1120w.avif';
import stream1120Webp from '@app/media/images/plutonium/feature-perk-stream-1120w.webp';
import stream1600Avif from '@app/media/images/plutonium/feature-perk-stream-1600w.avif';
import stream1600Webp from '@app/media/images/plutonium/feature-perk-stream-1600w.webp';
import upload640Avif from '@app/media/images/plutonium/feature-perk-upload-640w.avif';
import upload640Webp from '@app/media/images/plutonium/feature-perk-upload-640w.webp';
import upload1120Avif from '@app/media/images/plutonium/feature-perk-upload-1120w.avif';
import upload1120Webp from '@app/media/images/plutonium/feature-perk-upload-1120w.webp';
import upload1600Avif from '@app/media/images/plutonium/feature-perk-upload-1600w.avif';
import upload1600Webp from '@app/media/images/plutonium/feature-perk-upload-1600w.webp';
import stickerFeelThat from '@app/media/images/plutonium/sticker-feel-that.webp';
import type {MessageDescriptor} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import type React from 'react';

const PERK_SHOT_SIZES = '(min-width: 48rem) 50vw, calc(100vw - 5rem)';
const CHAT_SURFACE = 'rgb(30, 29, 35)';

interface ShotFamily {
	avif: [string, string, string];
	webp: [string, string, string];
}

interface PerkOverlay {
	src: string;
	left: number;
	top: number;
	width: number;
	height: number;
	backdrop?: string;
}

interface PerkCard {
	id: string;
	title: MessageDescriptor;
	body: MessageDescriptor;
	shots: ShotFamily;
	width: number;
	height: number;
	overlays: ReadonlyArray<PerkOverlay>;
	profileOverlay?: boolean;
}

const SHOT_WIDTHS = [640, 1120, 1600] as const;

const EXPRESSIONS_OVERLAYS: ReadonlyArray<PerkOverlay> = [
	{src: emojiCatvibe, left: 65.22, top: 11.241, width: 4.372, height: 5.128, backdrop: CHAT_SURFACE},
	{src: emojiCatwave, left: 14.936, top: 31.197, width: 8.743, height: 10.256, backdrop: CHAT_SURFACE},
	{src: stickerFeelThat, left: 14.936, top: 52.35, width: 29.144, height: 34.188},
];

const PERK_CARDS: ReadonlyArray<PerkCard> = [
	{
		id: 'expressions',
		title: SHOWCASE_EXPRESSIONS_TITLE_DESCRIPTOR,
		body: SHOWCASE_EXPRESSIONS_BODY_DESCRIPTOR,
		shots: {
			avif: [expressions640Avif, expressions1120Avif, expressions1600Avif],
			webp: [expressions640Webp, expressions1120Webp, expressions1600Webp],
		},
		width: 549,
		height: 468,
		overlays: EXPRESSIONS_OVERLAYS,
	},
	{
		id: 'profile',
		title: SHOWCASE_PROFILE_TITLE_DESCRIPTOR,
		body: SHOWCASE_PROFILE_BODY_DESCRIPTOR,
		shots: {
			avif: [profile640Avif, profile1120Avif, profile1600Avif],
			webp: [profile640Webp, profile1120Webp, profile1600Webp],
		},
		width: 662,
		height: 407,
		overlays: [],
		profileOverlay: true,
	},
	{
		id: 'stream',
		title: SHOWCASE_STREAM_TITLE_DESCRIPTOR,
		body: SHOWCASE_STREAM_BODY_DESCRIPTOR,
		shots: {
			avif: [stream640Avif, stream1120Avif, stream1600Avif],
			webp: [stream640Webp, stream1120Webp, stream1600Webp],
		},
		width: 588,
		height: 343,
		overlays: [],
	},
	{
		id: 'upload',
		title: SHOWCASE_UPLOAD_TITLE_DESCRIPTOR,
		body: SHOWCASE_UPLOAD_BODY_DESCRIPTOR,
		shots: {
			avif: [upload640Avif, upload1120Avif, upload1600Avif],
			webp: [upload640Webp, upload1120Webp, upload1600Webp],
		},
		width: 549,
		height: 362,
		overlays: [],
	},
];

function srcSet(urls: ReadonlyArray<string>): string {
	return urls.map((url, index) => `${url} ${SHOT_WIDTHS[index]}w`).join(', ');
}

function ProfileOverlay() {
	return (
		<svg className={styles.perkOverlaySvg} viewBox="0 0 662 407" aria-hidden="true" focusable="false">
			<defs>
				<clipPath id="plutonium-perk-banner-clip">
					<path d="M345.333 121.448V19.833a3.5 3.5 0 0 1 3.5-3.5h293.333a3.5 3.5 0 0 1 3.5 3.5v101.615z" />
				</clipPath>
				<mask id="plutonium-perk-banner-bite" maskUnits="userSpaceOnUse" x="0" y="0" width="662" height="407">
					<rect width="662" height="407" fill="#fff" />
					<circle cx="398.333" cy="114.333" r="43.2" fill="#000" />
				</mask>
				<clipPath id="plutonium-perk-avatar-clip">
					<circle cx="398.333" cy="114.333" r="40" />
				</clipPath>
				<mask id="plutonium-perk-avatar-notch" maskUnits="userSpaceOnUse" x="0" y="0" width="662" height="407">
					<rect width="662" height="407" fill="#fff" />
					<circle cx="426.333" cy="142.333" r="11.2" fill="#000" />
				</mask>
			</defs>
			<g clipPath="url(#plutonium-perk-banner-clip)">
				<g mask="url(#plutonium-perk-banner-bite)">
					<image
						href={bannerLsf}
						x="345.333"
						y="16.333"
						width="300.333"
						height="105.115"
						preserveAspectRatio="xMidYMid slice"
					/>
				</g>
				<circle cx="398.333" cy="114.333" r="41.6" fill="none" stroke="#0c0b0e" strokeWidth="3.2" />
			</g>
			<g clipPath="url(#plutonium-perk-avatar-clip)" mask="url(#plutonium-perk-avatar-notch)">
				<image href={avatarLsf} x="358.333" y="74.333" width="80" height="80" preserveAspectRatio="xMidYMid slice" />
			</g>
		</svg>
	);
}

function PerkArt({card, eager}: {card: PerkCard; eager: boolean}) {
	return (
		<div className={styles.perkArt} data-flx="premium.plutonium-page.perk-art">
			<picture>
				<source type="image/avif" srcSet={srcSet(card.shots.avif)} sizes={PERK_SHOT_SIZES} />
				<source type="image/webp" srcSet={srcSet(card.shots.webp)} sizes={PERK_SHOT_SIZES} />
				<img
					draggable={false}
					className={styles.perkArtImage}
					src={card.shots.webp[1]}
					width={card.width}
					height={card.height}
					loading={eager ? 'eager' : 'lazy'}
					fetchPriority={eager ? 'high' : undefined}
					decoding="async"
					alt=""
					data-flx="premium.plutonium-page.perk-art.image"
				/>
			</picture>
			{card.overlays.map((overlay) => (
				<img
					key={overlay.src}
					draggable={false}
					className={styles.perkOverlay}
					src={overlay.src}
					alt=""
					aria-hidden="true"
					loading="lazy"
					decoding="async"
					fetchPriority="low"
					style={{
						left: `${overlay.left.toFixed(3)}%`,
						top: `${overlay.top.toFixed(3)}%`,
						width: `${overlay.width.toFixed(3)}%`,
						height: `${overlay.height.toFixed(3)}%`,
						borderRadius: 0,
						backgroundColor: overlay.backdrop,
					}}
					data-flx="premium.plutonium-page.perk-art.overlay"
				/>
			))}
			{card.profileOverlay && <ProfileOverlay />}
		</div>
	);
}

interface PlutoniumPageShowcaseProps {
	uploadSize: string;
	freeUploadSize: string;
	footnoteId: string;
	onFootnoteClick: (event: React.MouseEvent<HTMLAnchorElement>) => void;
}

export function PlutoniumPageShowcase({
	uploadSize,
	freeUploadSize,
	footnoteId,
	onFootnoteClick,
}: PlutoniumPageShowcaseProps) {
	const {i18n} = useLingui();
	return (
		<>
			{PERK_CARDS.map((card, index) => {
				const flipped = index % 2 === 1;
				const body =
					card.id === 'profile'
						? renderProfileBody(
								i18n._(card.body, {footnote: '\u0000'}),
								footnoteId,
								onFootnoteClick,
								i18n._(TAG_FOOTNOTE_MARKER_DESCRIPTOR),
							)
						: i18n._(card.body, {freeSize: freeUploadSize});
				return (
					<article
						key={card.id}
						className={clsx(styles.glassPanel, styles.perkRow, flipped && styles.perkRowFlipped)}
						data-flx={`premium.plutonium-page.perk-row.${card.id}`}
					>
						<div className={styles.perkArtColumn} data-flx="premium.plutonium-page.perk-row.art-column">
							<PerkArt card={card} eager={index === 0} />
						</div>
						<div className={styles.perkTextColumn} data-flx="premium.plutonium-page.perk-row.text-column">
							<h2 className={styles.perkTitle} data-flx="premium.plutonium-page.perk-row.title">
								{i18n._(card.title, {size: uploadSize})}
							</h2>
							<p className={styles.perkBody} data-flx="premium.plutonium-page.perk-row.body">
								{body}
							</p>
						</div>
					</article>
				);
			})}
		</>
	);
}

function renderProfileBody(
	text: string,
	footnoteId: string,
	onFootnoteClick: (event: React.MouseEvent<HTMLAnchorElement>) => void,
	markerLabel: string,
): React.ReactNode {
	const [before, after = ''] = text.split('\u0000');
	return (
		<>
			<span>{before}</span>
			<FocusRing offset={-2}>
				<a
					href={`#${footnoteId}`}
					className={styles.footnoteMarker}
					onClick={onFootnoteClick}
					data-flx="premium.plutonium-page.perk-row.footnote-marker"
				>
					<span aria-hidden="true">*</span>
					<span className={styles.srOnly}>{markerLabel}</span>
				</a>
			</FocusRing>
			<span>{after}</span>
		</>
	);
}
