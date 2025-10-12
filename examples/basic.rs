use memory_spec::MemorySpec;

fn main() {
    let content = std::fs::read_to_string("examples/memory.kdl").unwrap();
    let memoryspec = match MemorySpec::from_str(&content) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let primary_slot = &memoryspec.regions()["appcore_flash"]["primary_slot"];
    let primary_slot_origin = primary_slot.origin();
    let primary_slot_length = primary_slot.length();
    let appcore_ram = &memoryspec.regions()["appcore_ram"]["low_ram"];
    let appcore_ram_origin = appcore_ram.origin();
    let appcore_ram_length = appcore_ram.length();
    let symbols = memoryspec.render_symbols();
    let memoryx = format!(
        "\
MEMORY
{{
  FLASH : ORIGIN = 0x{primary_slot_origin:08x}, LENGTH = 0x{primary_slot_length:04x}
  RAM : ORIGIN = 0x{appcore_ram_origin:08x}, LENGTH = 0x{appcore_ram_length:04x}
}}
{symbols}"
    );
    print!("{}", memoryx);
}
