-record(sysconf, {
    key,
    value,
    notes,
    created_on,
    created_by,
    modified_on,
    modified_by
   }).

-record(access, {
    id,
    name,
    notes,
    token,
    limits,
    status,
    created_on,
    modified_on
  }).

-record(roles,{
    id,
    name,
    notes,
    access_tokens,
    access_limits,
    status,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(users,{
    id,
    name,
    notes,
    email,
    password,
    access_expiry,
    role,
    role_expiry,
    revert_role,
    status,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(templates, {
    id,
    name,
    notes,
    label_pattern,
    seq_pattern,
    currency,
    face_value,
    deduction,
    actual_value,
    expiry,
    pin_type,
    pin_length,
    pin_pattern,
    attributes,
    batch_size,
    order_limits,
    usage_mode,
    status,
    created_on,
    created_by,
    modified_on,
    modified_by
  }).

-record(orders, {
    id,
    name,
    notes,
    imported_flag,
    label_fillers,
    seq_fillers,
    pin_template,
    pin_fillers,
    pin_count,
    batch_count,
    crypto_key,
    schedule,
    safe,
    status,
    created_on,
    created_by,
    approved_on,
    approved_by,
    generate_starttime,
    generate_endtime,
    loaded_on,
    loaded_by,
    activated_on,
    activited_by
  }).


-record(pins, {
    seq,
    pin,
    order,
    status,
    created_on,
    expires_on
  }).

-record(deadpins, {
    seq,
    pin,
    order,
    status,
    created_on,
    death_by,
    dead_on
  }).

-record(audit, {
    id,
    user,
    request,
    status,
    response,
    timestamp
  }).
