// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_AR_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "تم تعطيل حسابك في {product_name} مؤقتًا",
		"body": "مرحباً {username}،\n\nلقد قمنا بتعطيل حسابك في {product_name} مؤقتًا لأننا اكتشفنا نشاطًا مشبوهًا.\n\n{reason, select,\n  null {}\n  other {السبب: {reason}}\n}\n\nلاستعادة الوصول إلى حسابك، ستحتاج إلى إعادة تعيين كلمة المرور الخاصة بك:\n\n{forgotUrl}\n\nبعد إعادة تعيين كلمة المرور الخاصة بك، ستتمكن من تسجيل الدخول مرة أخرى.\n\nإذا كنت تعتقد أن هذا حدث عن طريق الخطأ، يرجى الاتصال بفريق الدعم لدينا.\n\n– فريق أمان {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "سيتم حذف حسابك في {product_name} نهائيًا",
		"body": "مرحباً {username}،\n\nتم جدولة حسابك في {product_name} للحذف الدائم بسبب انتهاكات لشروط الخدمة أو إرشادات المجتمع الخاصة بنا.\n\nالحذف المجدول: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {السبب: {reason}}\n}\n\nهذا إجراء تنفيذي جاد. سيتم حذف بيانات حسابك نهائيًا في التاريخ المحدد.\n\nيرجى مراجعة:\n- شروط الخدمة: {termsUrl}\n- إرشادات المجتمع: {guidelinesUrl}\n\nعملية الاستئناف:\nإذا كنت تعتقد أن قرار التنفيذ هذا كان غير صحيح أو غير مبرر، لديك 60 يومًا لتقديم استئناف. أرسل بريدًا إلكترونيًا إلى {appeals_email} من عنوان البريد الإلكتروني هذا.\n\nفي استئنافك:\n- اشرح بوضوح لماذا تعتقد أن قرار التنفيذ كان غير صحيح أو غير مبرر\n- قدم أي دليل أو سياق ذي صلة\n\nسيقوم عضو من فريق أمان {product_name} بمراجعة استئنافك وقد يوقف الحذف المعلق حتى يتم التوصل إلى قرار نهائي.\n\n– فريق أمان {product_name}"
	},
	"account_temp_banned": {
		"subject": "تم تعليق حسابك في {product_name} مؤقتًا",
		"body": "مرحباً {username}،\n\nتم تعليق حسابك في {product_name} مؤقتًا لانتهاك شروط الخدمة أو إرشادات المجتمع الخاصة بنا.\n\nالمدة: {durationHours, plural,\n  =1 {ساعة واحدة} zero {0 ساعات} two {ساعتان}\n  few {ساعات} many {ساعة} other {# ساعة}\n}\nمعلق حتى: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {السبب: {reason}}\n}\n\nخلال هذه الفترة، لن تتمكن من الوصول إلى حسابك.\n\nيرجى مراجعة:\n- شروط الخدمة: {termsUrl}\n- إرشادات المجتمع: {guidelinesUrl}\n\nإذا كنت تعتقد أن قرار التنفيذ هذا كان غير صحيح أو غير مبرر، يمكنك تقديم استئناف. أرسل بريدًا إلكترونيًا إلى {appeals_email} من عنوان البريد الإلكتروني هذا واشرح بوضوح لماذا تعتقد أن القرار كان غير صحيح. سنراجع استئنافك ونرد بقرارنا.\n\n– فريق أمان {product_name}"
	},
	"donation_confirmation": {
		"subject": "شكرًا لك على تبرعك لـ {product_name}",
		"body": "مرحباً،\n\nشكرًا لك على تبرعك لـ {product_name}! لقد تم {interval, select,\n  month {إعداد تبرعك المتكرر}\n  year {إعداد تبرعك المتكرر}\n  other {معالجة تبرعك لمرة واحدة}\n} بنجاح.\n\nتفاصيل التبرع:\nالمبلغ: {amount} {currency} {interval, select,\n  month {شهريًا}\n  year {سنويًا}\n  other {}\n}\n\nسترسل لك Stripe إيصالًا منفصلاً عبر البريد الإلكتروني مع فاتورتك بصيغة PDF قريبًا. يتضمن هذا جميع تفاصيل الدفع ويمكن استخدامه لأغراض ضريبية.\n\nيمكنك عرض سجل تبرعاتك، وتنزيل الفواتير، {interval, select,\n  month {وإدارة أو إلغاء اشتراكك}\n  year {وإدارة أو إلغاء اشتراكك}\n  other {وإدارة التبرعات المستقبلية}\n} في أي وقت باستخدام هذا الرابط:\n\n{manageUrl}\n\nدعمك يساعد في استمرار {product_name}. شكرًا لك!\n\n– فريق {product_name}{interval, select,\n  month {}\n  year {}\n  other {}\n}"
	},
	"donation_magic_link": {
		"subject": "إدارة تبرعات {product_name}",
		"body": "مرحباً،\n\nانقر على الرابط أدناه للوصول إلى بوابة المتبرعين الخاصة بك:\n\n{manageUrl}\n\nفي البوابة، يمكنك إدارة الاشتراكات، وتنزيل الفواتير، وعرض سجل تبرعاتك.\n\nينتهي صلاحية هذا الرابط في {expiresAt, date, full} الساعة {expiresAt, time, short}.\n\nإذا لم تطلب هذا الرابط، يمكنك تجاهل هذا البريد الإلكتروني بأمان.\n\n– فريق {product_name}"
	},
	"dsa_report_verification": {
		"subject": "تحقق من بريدك الإلكتروني لتقديم تقرير DSA",
		"body": "مرحباً،\n\nاستخدم رمز التحقق أدناه لتقديم تقرير قانون الخدمات الرقمية الخاص بك على {product_name}:\n\n{code}\n\nينتهي هذا الرمز في {expiresAt, date, full} الساعة {expiresAt, time, short}.\n\nإذا لم تطلب هذا، يمكنك تجاهل هذا البريد الإلكتروني.\n\n– فريق أمان {product_name}"
	},
	"email_change_new": {
		"subject": "تحقق من بريدك الإلكتروني الجديد في {product_name}",
		"body": "مرحباً {username}،\n\nأدخل هذا الرمز في التطبيق للتحقق من بريدك الإلكتروني الجديد في {product_name}:\n\n{code}\n\nينتهي هذا الرمز في {expiresAt, date, full} الساعة {expiresAt, time, short}.\n\nإذا لم تطلب هذا، يمكنك تجاهل هذا البريد الإلكتروني.\n\n– فريق {product_name}"
	},
	"email_change_original": {
		"subject": "تأكيد تغيير بريدك الإلكتروني في {product_name}",
		"body": "مرحباً {username}،\n\nلقد تلقينا طلبًا لتغيير عنوان البريد الإلكتروني لحسابك في {product_name}.\n\nلتأكيد هذا التغيير، أدخل هذا الرمز في التطبيق:\n\n{code}\n\nينتهي هذا الرمز في {expiresAt, date, full} الساعة {expiresAt, time, short}.\n\nإذا لم تطلب هذا، يرجى تأمين حسابك على الفور.\n\n– فريق {product_name}"
	},
	"email_change_revert": {
		"subject": "تم تغيير بريدك الإلكتروني في {product_name}",
		"body": "مرحباً {username}،\n\nتم تغيير عنوان البريد الإلكتروني لحسابك في {product_name} إلى {newEmail}.\n\nإذا قمت بهذا التغيير، فلا داعي لاتخاذ أي إجراء. إذا لم تقم بذلك، يمكنك التراجع عن التغيير وتأمين حسابك باستخدام هذا الرابط:\n\n{revertUrl}\n\nسيؤدي هذا إلى استعادة بريدك الإلكتروني السابق، وتسجيل خروجك من جميع الأجهزة، وإزالة أرقام الهواتف المرتبطة، وتعطيل المصادقة متعددة العوامل، وسيطلب منك تعيين كلمة مرور جديدة.\n\n– فريق أمان {product_name}"
	},
	"email_verification": {
		"subject": "تحقق من بريدك الإلكتروني في {product_name}",
		"body": "مرحباً {username}،\n\nيرجى التحقق من عنوان البريد الإلكتروني لحسابك في {product_name} بالنقر على الرابط أدناه:\n\n{verifyUrl}\n\nإذا لم تقم بإنشاء حساب {product_name}، يمكنك تجاهل هذا البريد الإلكتروني بأمان.\n\nهذا الرابط صالح لمدة 24 ساعة.\n\n– فريق {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "تمت إزالة المزايا من هديتك",
		"body": "مرحباً {username}،\n\nرمز الهدية الذي استرددته تم دفعه في الأصل من قبل شخص آخر. وقد تم عكس هذا الدفع منذ ذلك الحين (استرداد المبلغ).\n\nبسبب هذا، قمنا بإزالة المزايا التي أضيفت إلى حسابك عندما استرددت الهدية.\n\nإذا كنت تعتقد أن هذا خطأ، يرجى الاتصال بفريق الدعم لدينا وتضمين أي تفاصيل لديك حول رمز الهدية ومتى استرددته.\n\n– فريق {product_name}"
	},
	"harvest_completed": {
		"subject": "تصدير بياناتك من {product_name} جاهز للتنزيل",
		"body": "مرحباً {username}،\n\nتصدير بياناتك جاهز.\n\nرابط التنزيل:\n{downloadUrl}\n\nالرسائل المتضمنة: {totalMessages, number}\nحجم الملف: {fileSizeMB, number} ميجابايت\n\nينتهي هذا الرابط في {expiresAt, date, full} الساعة {expiresAt, time, short}.\n\nإذا لم تطلب هذا التصدير، يرجى تغيير كلمة المرور الخاصة بك على الفور والاتصال بفريق الدعم لدينا.\n\n– فريق {product_name}"
	},
	"inactivity_warning": {
		"subject": "سيتم حذف حسابك في {product_name} بسبب عدم النشاط",
		"body": "مرحباً {username}،\n\nلم نرَ أي نشاط على حسابك في {product_name} منذ {lastActiveDate, date, full}.\n\nإذا لم تسجل الدخول بحلول {deletionDate, date, full} الساعة {deletionDate, time, short}، فسيتم حذف حسابك نهائيًا بسبب عدم النشاط.\n\nسجل الدخول هنا:\n{loginUrl}\n\nإذا كنت قد استخدمت {product_name} مؤخرًا، يرجى الاتصال بفريق الدعم لدينا على الفور.\n\n– فريق {product_name}"
	},
	"ip_authorization": {
		"subject": "السماح بتسجيل الدخول من عنوان IP جديد",
		"body": "مرحباً {username}،\n\nلقد اكتشفنا محاولة تسجيل دخول إلى حسابك في {product_name} من عنوان IP جديد:\n\nعنوان IP: {ipAddress}\nالموقع: {location}\n\nإذا كنت أنت من قام بذلك، يرجى تفويض عنوان IP هذا بالنقر على الرابط أدناه:\n\n{authUrl}\n\nإذا لم تحاول تسجيل الدخول، يرجى تغيير كلمة المرور الخاصة بك على الفور.\n\nهذا الرابط صالح لـ 30 دقيقة.\n\n– فريق {product_name}"
	},
	"password_change_verification": {
		"subject": "تأكيد تغيير كلمة المرور في {product_name}",
		"body": "مرحباً {username}،\n\nلقد تلقينا طلبًا لتغيير كلمة المرور لحسابك في {product_name}.\n\nلتأكيد هذا التغيير، أدخل هذا الرمز في التطبيق:\n\n{code}\n\nينتهي هذا الرمز في {expiresAt}.\n\nإذا لم تطلب هذا، فقد يكون شخص ما قد وصل إلى حسابك. قم بتغيير كلمة المرور الخاصة بك على الفور وقم بتمكين المصادقة الثنائية.\n\n– فريق {product_name}"
	},
	"password_reset": {
		"subject": "إعادة تعيين كلمة المرور في {product_name}",
		"body": "مرحباً {username}،\n\nلقد طلبت إعادة تعيين كلمة المرور الخاصة بك في {product_name}. استخدم الرابط أدناه لتعيين كلمة مرور جديدة:\n\n{resetUrl}\n\nإذا لم تطلب هذا، يمكنك تجاهل هذا البريد الإلكتروني بأمان.\n\nهذا الرابط صالح لساعة واحدة.\n\n– فريق {product_name}"
	},
	"registration_approved": {
		"subject": "تمت الموافقة على تسجيلك في {product_name}",
		"body": "مرحباً {username}،\n\nأخبار سارة: تمت الموافقة على تسجيلك في {product_name}.\n\nيمكنك الآن تسجيل الدخول إلى تطبيق {product_name} من هنا:\n{channelsUrl}\n\nمرحباً بك في مجتمع {product_name}.\n\n– فريق {product_name}"
	},
	"report_resolved": {
		"subject": "تمت مراجعة تقريرك في {product_name}",
		"body": "مرحباً {username}،\n\nتمت مراجعة تقريرك (المعرف: {reportId}) من قبل فريق الأمان لدينا.{hasComment, select, yes {\n\nرد فريق الأمان:\n{publicComment}} other {}}\n\nشكرًا لمساعدتك في الحفاظ على {product_name} آمنًا للجميع. نحن نأخذ جميع التقارير على محمل الجد ونقدر مساهمتك في المجتمع.\n\nإذا كان لديك أي أسئلة أو مخاوف بشأن هذه النتيجة، يرجى الاتصال بـ {safety_email}.\n\n– فريق أمان {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "سيتم حذف حسابك في {product_name} نهائيًا",
		"body": "مرحباً {username}،\n\nتم جدولة حسابك في {product_name} للحذف الدائم.\n\nالحذف المجدول: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {السبب: {reason}}\n}\n\nهذا إجراء تنفيذي جاد. سيتم حذف بيانات حسابك نهائيًا في التاريخ المحدد.\n\nإذا كنت تعتقد أن قرار التنفيذ هذا كان غير صحيح، يمكنك تقديم استئناف. أرسل بريدًا إلكترونيًا إلى {appeals_email} من عنوان البريد الإلكتروني هذا.\n\n– فريق أمان {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "تم جدولة حذف حسابك في {product_name}",
		"body": "مرحباً {username}،\n\nلقد طلبت حذف حسابك في {product_name}. تم جدولة حسابك للحذف الدائم في:\n\n{deletionDate, date, full} الساعة {deletionDate, time, short}\n\nإذا لم تطلب هذا، سجل الدخول إلى حسابك لإلغاء الحذف. نوصي أيضًا بتغيير كلمة المرور الخاصة بك لتأمين حسابك.\n\n– فريق {product_name}"
	},
	"unban_notification": {
		"subject": "تم رفع تعليق حسابك في {product_name}",
		"body": "مرحباً {username}،\n\nأخبار سارة: تم رفع تعليق حسابك في {product_name}.\n\n{reason, select,\n  null {}\n  other {السبب: {reason}}\n}\n\nيمكنك الآن تسجيل الدخول مرة أخرى ومواصلة استخدام {product_name} كالمعتاد.\n\n– فريق أمان {product_name}"
	}
});

export default EMAIL_I18N_AR_MESSAGES;
