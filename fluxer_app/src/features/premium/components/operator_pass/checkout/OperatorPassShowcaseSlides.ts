// SPDX-License-Identifier: AGPL-3.0-or-later

import badgeAvif from '@app/media/images/operator_pass/showcase/badge-640w.avif';
import badgeWebp from '@app/media/images/operator_pass/showcase/badge-640w.webp';
import certificateAvif from '@app/media/images/operator_pass/showcase/certificate-640w.avif';
import certificateWebp from '@app/media/images/operator_pass/showcase/certificate-640w.webp';
import operatorsAvif from '@app/media/images/operator_pass/showcase/operators-640w.avif';
import operatorsWebp from '@app/media/images/operator_pass/showcase/operators-640w.webp';
import testingAvif from '@app/media/images/operator_pass/showcase/testing-640w.avif';
import testingWebp from '@app/media/images/operator_pass/showcase/testing-640w.webp';
import wallAvif from '@app/media/images/operator_pass/showcase/wall-640w.avif';
import wallWebp from '@app/media/images/operator_pass/showcase/wall-640w.webp';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export interface OperatorPassShowcaseSlide {
	id: string;
	avif: string;
	webp: string;
	width: number;
	height: number;
	title: MessageDescriptor;
	body: MessageDescriptor;
	alt: MessageDescriptor | null;
}

export const OPERATOR_PASS_SHOWCASE_SLIDES: ReadonlyArray<OperatorPassShowcaseSlide> = [
	{
		id: 'certificate',
		avif: certificateAvif,
		webp: certificateWebp,
		width: 640,
		height: 453,
		title: msg({
			message: 'Your Operator certificate',
			comment:
				'Heading of the first slide in the Operator Pass checkout dialog, next to a picture of a sample certificate. The certificate is a PDF with one page for each pass, and each page has its own number, the name chosen at checkout and the date and time of payment. Always call it a certificate, never a diploma, card, collectible, token or license, and never suggest it can be resold or has cash value. Keep Operator in English.',
		}),
		body: msg({
			message:
				"Your pass comes with this certificate. It's a PDF with one page for each pass, and each page has its own number, the name you choose at checkout and the date and time you paid. We email it to you, and in settings each page has its own link that you can open, print or share. Buying for someone else? Put their name on their certificate.",
			comment:
				"Text under the heading Your Operator certificate on a slide in the Operator Pass checkout dialog. The certificate is a PDF with one page for each pass, and each page has its own number, the name chosen at checkout and the date and time of payment. Always call it a certificate, never a diploma, card, collectible, token or license, and never suggest it can be resold or has cash value. Keep Operator in English. Checkout is where you pay. Settings are the app settings, where each certificate page has its own private link. The last two sentences tell a buyer who buys a pass as a gift to type the other person's name on that page. Calm and plain.",
		}),
		alt: null,
	},
	{
		id: 'badge',
		avif: badgeAvif,
		webp: badgeWebp,
		width: 640,
		height: 697,
		title: msg({
			message: 'The Operator badge',
			comment:
				"Heading of a slide in the Operator Pass checkout dialog about the badge shown on the user's profile. Operator is the badge's name, keep it in English. No trailing punctuation.",
		}),
		body: msg({
			message:
				'The badge on your Fluxer profile shows when you got your pass, like "Operator Pass since November 2026". You can hide it, or just its date, in your settings at any time.',
			comment:
				'Text under the heading The Operator badge on a slide in the Operator Pass checkout dialog. The quoted example is the text on the badge, translate it the way the app shows it in your language. Operator badge is a name, keep Operator in English. Never add a word that says how long the badge lasts.',
		}),
		alt: null,
	},
	{
		id: 'wall',
		avif: wallAvif,
		webp: wallWebp,
		width: 640,
		height: 677,
		title: msg({
			message: "A spot on the Operators' wall",
			comment:
				"Heading of a slide in the Operator Pass checkout dialog about the Operators' wall. The Operators' wall is a section on the website's homepage that shows Operators, the people who have an Operator Pass, with their avatars and names. Keep the word Operators in English. Never suggest the spot belongs to the buyer or is what the price pays for.",
		}),
		body: msg({
			message:
				"If you'd like a spot, it goes up with your name and avatar when you pay, and you can leave the wall or come back in your settings.",
			comment:
				"Text under the heading A spot on the Operators' wall on a slide in the Operator Pass checkout dialog. The Operators' wall is a section on the website's homepage that shows Operators, the people who have an Operator Pass, with their avatars and names. Keep the word Operators in English. Never add a word that says how long the spot lasts.",
		}),
		alt: null,
	},
	{
		id: 'operators',
		avif: operatorsAvif,
		webp: operatorsWebp,
		width: 640,
		height: 495,
		title: msg({
			message: '#operators in Fluxer Labs',
			comment:
				"Heading of a slide in the Operator Pass checkout dialog about the #operators channel. #operators is the channel's literal name and Fluxer Labs is the name of the official community, keep both unchanged. No trailing punctuation.",
		}),
		body: msg({
			message:
				'The pass opens #operators to talk about self-hosting, bots and the API. Hampus, who created {productName}, joins in when he can.',
			comment:
				"Text under the heading #operators in Fluxer Labs on a slide in the Operator Pass checkout dialog. The pass is the Operator Pass. #operators is the channel's literal name, keep it unchanged. Opens #operators to talk about means the pass gives access to the channel, where people talk about self-hosting, bots and the API. Hampus is a person's first name, keep it as is. Who created {productName} describes him as the person who started and made the software. Use past tense. Joins in when he can means Hampus takes part in the conversation when he has time, never that he answers questions or gives support. Warm and plain, never salesy. No dashes as punctuation and no semicolons.",
		}),
		alt: msg({
			message: 'Operators and the Fluxer Team talking about self-hosting in the #operators channel of Fluxer Labs',
			comment:
				"Alternative text of the screenshot on a slide in the Operator Pass checkout dialog, read out by screen readers. The screenshot shows a short chat in the #operators channel. Operators are the people who have an Operator Pass, keep the word Operators in English. Fluxer Labs is the name of the official community, keep it unchanged. #operators is the channel's literal name, keep it unchanged. Talking means chatting with each other, never asking for or giving support. No trailing punctuation.",
		}),
	},
	{
		id: 'testing',
		avif: testingAvif,
		webp: testingWebp,
		width: 640,
		height: 640,
		title: msg({
			message: 'Early feature testing',
			comment:
				'Heading of a slide in the Operator Pass checkout dialog about being invited to try new features before they are released. Plain, no hype. No trailing punctuation.',
		}),
		body: msg({
			message:
				"When we have a new {productName} feature ready for testing, we may invite you to try it before it's released and to tell us what you think of it. Features that haven't been released yet can still have bugs.",
			comment:
				"Text under the heading Early feature testing on a slide in the Operator Pass checkout dialog. When the team has a new feature ready for testing, we may invite people with the pass to try it before it is released and to say what they think of it. May invite means it is an invitation we can choose to send, never a promise that every Operator tests every feature. The second sentence says features that haven't been released yet can still have bugs. Calm and plain, no hype. No dashes as punctuation and no semicolons.",
		}),
		alt: null,
	},
];
