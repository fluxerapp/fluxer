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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import type {MarketingContext} from '@fluxer/marketing/src/MarketingContext';
import {escapeInlineScriptValue} from '@fluxer/marketing/src/pages/InlineScriptEscaping';

export interface DonationManageI18n {
	title: string;
	description: string;
	emailPlaceholder: string;
	sendLink: string;
	sending: string;
	success: string;
	errorInvalidEmail: string;
	errorGeneric: string;
	errorNetwork: string;
}

export function getDonationManageI18n(ctx: MarketingContext): DonationManageI18n {
	return {
		title: ctx.i18n.getMessage('donations.manage.title', ctx.locale),
		description: ctx.i18n.getMessage('donations.manage.description', ctx.locale),
		emailPlaceholder: ctx.i18n.getMessage('donations.form.email_placeholder', ctx.locale),
		sendLink: ctx.i18n.getMessage('donations.form.send_link', ctx.locale),
		sending: ctx.i18n.getMessage('donations.form.sending', ctx.locale),
		success: ctx.i18n.getMessage('donations.manage.success', ctx.locale),
		errorInvalidEmail: ctx.i18n.getMessage('donations.errors.invalid_email', ctx.locale),
		errorGeneric: ctx.i18n.getMessage('donations.errors.generic', ctx.locale),
		errorNetwork: ctx.i18n.getMessage('donations.errors.network', ctx.locale),
	};
}

export function renderDonationManageForm(ctx: MarketingContext, emailParam: string): JSX.Element {
	const apiEndpoint = ctx.apiEndpoint;
	const i18n = getDonationManageI18n(ctx);

	return (
		<>
			<div class="mx-auto flex max-w-md gap-2">
				<input
					type="email"
					id="manage-email"
					placeholder={i18n.emailPlaceholder}
					value={emailParam}
					class="flex-1 rounded-lg border border-gray-200 px-4 py-2"
				/>
				<button
					type="button"
					id="send-link-btn"
					onclick="requestManageLink()"
					class="rounded-lg bg-gray-800 px-6 py-2 font-medium text-white transition hover:bg-gray-700"
				>
					{i18n.sendLink}
				</button>
			</div>
			<p id="manage-message" class="mt-2 hidden text-center text-sm" />

			<script
				dangerouslySetInnerHTML={{
					__html: `var manageI18n = {
  sendLink: '${escapeInlineScriptValue(i18n.sendLink)}',
  sending: '${escapeInlineScriptValue(i18n.sending)}',
  success: '${escapeInlineScriptValue(i18n.success)}',
  errorInvalidEmail: '${escapeInlineScriptValue(i18n.errorInvalidEmail)}',
  errorGeneric: '${escapeInlineScriptValue(i18n.errorGeneric)}',
  errorNetwork: '${escapeInlineScriptValue(i18n.errorNetwork)}'
};

async function requestManageLink() {
  var email = document.getElementById('manage-email').value;
  var msgEl = document.getElementById('manage-message');
  var btn = document.getElementById('send-link-btn');

  if (!email || !email.includes('@')) {
    msgEl.textContent = manageI18n.errorInvalidEmail;
    msgEl.className = 'mt-2 text-center text-sm text-red-500';
    msgEl.classList.remove('hidden');
    return;
  }

  btn.disabled = true;
  btn.textContent = manageI18n.sending;

  try {
    var response = await fetch('${apiEndpoint}/donations/request-link', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({email: email})
    });

    if (response.ok) {
      msgEl.textContent = manageI18n.success;
      msgEl.className = 'mt-2 text-center text-sm text-green-600';
    } else {
      var error = await response.json().catch(function() { return {}; });
      msgEl.textContent = error.message || manageI18n.errorGeneric;
      msgEl.className = 'mt-2 text-center text-sm text-red-500';
    }
  } catch (err) {
    msgEl.textContent = manageI18n.errorNetwork;
    msgEl.className = 'mt-2 text-center text-sm text-red-500';
  }

  msgEl.classList.remove('hidden');
  btn.disabled = false;
  btn.textContent = manageI18n.sendLink;
}`,
				}}
			/>
		</>
	);
}
